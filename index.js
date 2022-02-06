const _ = require('lodash');
const { round, ceil, floor, sqrt, abs, min, max } = Math;

function bounce(err) {
	throw Error(err);
}

function require_cond(cond, err) {
	if (!cond)
		throw Error(err);
}

let log = () => { }
function setLogger(logger) {
	log = logger;
}

const is_integer = value => typeof value === 'number' && isFinite(value) && floor(value) === value

const timestamp = () => round(Date.now() / 1000)


// AA functions

const $get_leverages = () => [2, 5, 10, 20, 50, 100];

const $singularity_threshold = 0.1;
const $trade_merge_period = 1;


const $update_recent_data = ($recent, $p, $final_p, $trigger_initial_address, $tax_token, $traded_amount, $paid_tax, $period_length) => {
	const $period_start_ts = floor(timestamp() / $period_length) * $period_length;
	const $pmin = min($p, $final_p);
	const $pmax = max($p, $final_p);
	if (!$recent.current || +$recent.current.start_ts < $period_start_ts){
		$recent.prev = $recent.current;
		$recent.current = {start_ts: $period_start_ts, pmin: $pmin, pmax: $pmax};
	}
	else{
		$recent.current.pmin = min($recent.current.pmin, $pmin);
		$recent.current.pmax = max($recent.current.pmax, $pmax);
	}
	if ($recent.last_trade && $recent.last_trade.address == $trigger_initial_address && $recent.last_ts >= timestamp() - $trade_merge_period){ // closely following trades are merged into one trade
		$recent.last_trade.pmin = min($recent.last_trade.pmin, $pmin);
		$recent.last_trade.pmax = max($recent.last_trade.pmax, $pmax);
		$recent.last_trade.amounts[$tax_token] = $recent.last_trade.amounts[$tax_token] + $traded_amount;
		$recent.last_trade.paid_taxes[$tax_token] = $recent.last_trade.paid_taxes[$tax_token] + $paid_tax;
	}
	else{
		const $amounts = {x:0, y:0};
		const $paid_taxes = {x:0, y:0};
		$amounts[$tax_token] = $traded_amount;
		$paid_taxes[$tax_token] = $paid_tax;
		$recent.last_trade = {
			address: $trigger_initial_address,
			pmin: $pmin,
			pmax: $pmax,
			amounts: $amounts,
			paid_taxes: $paid_taxes,
		};
	}
	$recent.last_ts = timestamp();
};

const $get_utilization_ratio = ($balances, $l_balances, $x0, $y0, $alpha) => {
	const $beta = 1 - $alpha;
	const $ratio = $get_leverages().reduce(($acc, $L) =>
		$acc +
		($l_balances[$L + 'x'] ? $l_balances[$L + 'x'].balance : 0) / ($balances.x + $x0) * ($L - 1) / $beta +
		($l_balances[-$L + 'x'] ? $l_balances[-$L + 'x'].balance : 0) / ($balances.y + $y0) * ($L - 1) / $alpha,
	0);
	return $ratio
};

// X through Y:

// without LP leverage (Lambda)
const $get_final_x = ($X, $Y, $final_Y, $X0, $Y0, $pool_props, $inverted) => {
	require_cond($final_Y >= $Y, "not selling Y");
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_X = ($X + $X0) * (($Y + $Y0)/($final_Y + $Y0))**($b/$a) - $X0;
	require_cond($final_X >= 0, "bad final_X " + $final_X);
	const $deltaX = $X - $final_X;
	require_cond($deltaX >= 0, "bad deltaX " + $deltaX);
	return $final_X
};

// along x means keeping x fully leveraged (y underleveraged)
const $get_final_x_along_x = ($X, $Y, $final_Y, $pool_props, $inverted) => {
	log('get_final_x_along_x', $X, $Y, $final_Y);
	const $b = $inverted ? $pool_props.alpha : $pool_props.beta; // beta
	return $X * ($final_Y/$Y)**($b * $pool_props.Lambda/($b * $pool_props.Lambda - 1))
};

// along y means keeping y fully leveraged (x underleveraged)
const $get_final_x_along_y = ($X, $Y, $final_Y, $pool_props, $inverted) => {
	log('get_final_x_along_y', $X, $Y, $final_Y);
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	return $X * ($final_Y/$Y)**(1-1/$a/$pool_props.Lambda)
};


// Y through X:

// without LP leverage (Lambda)
const $get_final_y = ($X, $Y, $final_X, $X0, $Y0, $pool_props, $inverted) => {
//	require_cond($final_X <= $X, "not buying X"); // selling when redeeming L-tokens
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_Y = ($Y + $Y0) * (($X + $X0)/($final_X + $X0))**($a/$b) - $Y0;
	require_cond($final_Y >= 0, "bad final_Y " + $final_Y);
//	$deltaY = $final_Y - $Y;
//	require_cond($deltaY >= 0, "bad deltaY " + $deltaY);
	return $final_Y
};

// along x means keeping x fully leveraged (y underleveraged)
const $get_final_y_along_x = ($X, $Y, $final_X, $pool_props, $inverted) => {
	const $b = $inverted ? $pool_props.alpha : $pool_props.beta; // beta
	return $Y * ($final_X/$X)**(1 - 1/$b/$pool_props.Lambda)
};

// along y means keeping y fully leveraged (x underleveraged)
const $get_final_y_along_y = ($X, $Y, $final_X, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	return $Y * ($final_X/$X)**($a*$pool_props.Lambda/($a*$pool_props.Lambda - 1))
};
	
	

// X, Y through P:

// without LP leverage (Lambda)
const $get_final_xy = ($X, $Y, $P, $final_P, $X0, $Y0, $pool_props, $inverted) => {
	require_cond($final_P >= $P, "not selling Y");
//	log('get_final_xy', $X, $Y, $P, $final_P, $X0, $Y0);
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_X = ($X + $X0) * ($P/$final_P)**$b - $X0;
	const $final_Y = $b/$a * $final_P * ($final_X + $X0) - $Y0;
	const $deltaX = $X - $final_X;
	require_cond($final_X >= 0, "bad final_X " + $final_X);
	require_cond($final_Y >= 0, "bad final_Y " + $final_Y);
	require_cond($deltaX >= 0, "bad deltaX " + $deltaX);
	return {
		X: $final_X,
		Y: $final_Y,
	}
};

// along x means keeping x fully leveraged (y underleveraged)
const $get_final_xy_along_x = ($X, $Y, $P, $final_P, $pool_props, $inverted) => {
	require_cond($final_P >= $P, "along X: not selling Y");
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_X = $X * ($P/$final_P)**($b*$pool_props.Lambda);
	const $final_Y = $b/$a * $final_P * $final_X;
	return {
		X: $final_X,
		Y: $final_Y,
	}
};

// along y means keeping y fully leveraged (x underleveraged)
const $get_final_xy_along_y = ($X, $Y, $P, $final_P, $pool_props, $inverted) => {
	require_cond($final_P >= $P, "along Y: not selling Y");
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_Y = $Y * ($final_P/$P)**($a*$pool_props.Lambda);
	const $final_X = $a/$b * $final_Y / $final_P;
	return {
		X: $final_X,
		Y: $final_Y,
	}
};



const $add_net_balance_without_changing_price = ($balances, $profits, $side, $amount, $Lambda) => {
	if (!$amount)
		return;
//	bounce('add_net_balance_without_changing_price ' + $side + ' ' + $amount);
	if ($Lambda == 1){
		$profits[$side] = $profits[$side] + $amount;
		return;
	}

	const $opposite = $side == 'x' ? 'y' : 'x';
	const $side_n = $side + 'n';
	const $opposite_n = $opposite + 'n';
	
	const $Xn = $balances[$side_n];
	const $Yn = $balances[$opposite_n];
	const $X = $balances[$side];
	const $Y = $balances[$opposite];
	
	const $underleveraged = $Xn > ceil($X/$Lambda);
	const $delta_Xn = $amount;
//	const $delta_Yn = 0;
	let $delta_X, $delta_Y;
	// the price doesn't change as X and Y grow proportionally
	if (!$underleveraged){ // along X
		// Y is underleveraged, increase Y proportionally while keeping Yn intact
		const $full_delta_Y = $Y * $delta_Xn/$Xn;
		if ($Y + $full_delta_Y > $Yn * $Lambda){ // would overshoot and make Y overleveraged
			const $ratio = $Yn * $Lambda / $Y - 1;
			$delta_X = $ratio * $X;
			$delta_Y = $ratio * $Y;
		}
		else{
			$delta_X = $delta_Xn * $Lambda;
			$delta_Y = $full_delta_Y;
		}
	}
	else{
		$delta_X = 0; // only net X gets increased
		$delta_Y = 0;
	}

	$balances[$side_n] = $balances[$side_n] + $delta_Xn;
//	$balances[$opposite_n] = $balances[$opposite_n] + $delta_Yn;
	$balances[$side] = $balances[$side] + $delta_X;
	$balances[$opposite] = $balances[$opposite] + $delta_Y;
};


const $pow = ($precomputed, $power) => {
	require_cond($precomputed[$power], "no precomputed power " + $power);
	return $precomputed[$power]
};
const $precompute = $v => {
	let $pre = {};
	$pre['2'] = $v * $v;
	$pre['5'] = $pre['2'] * $pre['2'] * $v;
	$pre['10'] = $pre['5'] * $pre['5'];
	$pre['20'] = $pre['10'] * $pre['10'];
	$pre['50'] = $pre['20'] * $pre['20'] * $pre['10'];
	$pre['100'] = $pre['50'] * $pre['50'];
	return $pre
};

const $charge_interest = ($balances, $l_balances, $profits, $x0, $y0, $last_ts, $i, $alpha, $Lambda) => {
	require_cond($last_ts, "no last ts");
	if (Object.keys($l_balances).length == 0 || $i == 0 || timestamp() == $last_ts)
		return;
	const $beta = 1 - $alpha;
	const $x = $balances.x;
	const $y = $balances.y;
	
	const $accrued_rate = (timestamp() - $last_ts)/3600/24/360 * $i;
	const $factor = max(1 - $accrued_rate, 0); // how much is left
	const $factor_powers = $precompute($factor);

	const $y2x = ($y + $y0) / ($x + $x0);
	const $p = $alpha/$beta * $y2x;
	let $n_deltas = {dxn:0, dyn:0};
	// we change x and y in such a way that the price does not change
	$get_leverages().forEach(($L) => {
		const $xL = $l_balances[$L+'x'] ? $l_balances[$L+'x'].balance : 0;
		const $yL = $l_balances[-$L+'x'] ? $l_balances[-$L+'x'].balance : 0;
		if (!$xL && !$yL)
			return;
		const $factorL1 = $pow($factor_powers, $L) / $factor; // $factor**($L-1)
		if ($xL){
			const $dxL = -$xL * (1 - $factorL1);
			$l_balances[$L+'x'].balance = $xL + $dxL;
			// change in the amount lent out by the swap pool (swap pool's assets)
			const $delta_yn = $dxL * $p * ($L-1)/$L; // < 0
			$n_deltas.dyn = $n_deltas.dyn + $delta_yn;
			if ($Lambda == 1){
				$n_deltas.dxn = $n_deltas.dxn + $delta_yn / $y2x; // proportional - no price change
				$profits.x = $profits.x - $dxL - $delta_yn / $y2x; // income from interest + transferred from the pool to keep the price
			}
			else
				$n_deltas.dxn = $n_deltas.dxn - $dxL; // > 0
		}
		if ($yL){
			const $dyL = -$yL * (1 - $factorL1);
			$l_balances[-$L+'x'].balance = $yL + $dyL;
			// change in the amount lent out by the swap pool (swap pool's assets)
			const $delta_xn = $dyL / $p * ($L-1)/$L; // < 0
			$n_deltas.dxn = $n_deltas.dxn + $delta_xn; 
			if ($Lambda == 1){
				$n_deltas.dyn = $n_deltas.dyn + $delta_xn * $y2x; // proportional - no price change
				$profits.y = $profits.y - $dyL - $delta_xn * $y2x; // income from interest + transferred from the pool to keep the price
			}
			else
				$n_deltas.dyn = $n_deltas.dyn - $dyL; // > 0
		}
	});
//	log('interest', $n_deltas);

	const $dxn = $n_deltas.dxn;
	const $dyn = $n_deltas.dyn;

	if ($Lambda == 1){
		$balances.xn = $balances.xn + $dxn;
		$balances.yn = $balances.yn + $dyn;
		$balances.x = $balances.x + $dxn;
		$balances.y = $balances.y + $dyn;
	}
	else{
		$add_net_balance_without_changing_price($balances, $profits, 'x', $dxn, $Lambda);
		$add_net_balance_without_changing_price($balances, $profits, 'y', $dyn, $Lambda);
	}
};




const $update_leveraged_balances = ($l_balances, $P, $final_P, $inverted) => {
	const $ratio = $final_P/$P;
	const $ratio_powers = $precompute($ratio);

	let $totals = {
		delta_XL: 0, // (L>0) X added to the L-pools (bought from the swap pool) minus (L<0) new X borrowed by the L-pools (sent to the swap pool for buying Y)
		delta_YL: 0, // (L>0) Y added to the L-pools (bought from the swap pool) minus (L<0) new Y borrowed by the L-pools (sent to the swap pool for buying X)
		XL_denom: 0,
		YL_denom: 0,
	}; // if inverted, XL corresponds to y, YL to x
	$get_leverages().forEach(($L) => {
		const $allyL = $inverted ? -$L : $L;
		const $balance = $l_balances[$allyL+'x'] ? $l_balances[$allyL+'x'].balance : 0;
		const $obalance = $l_balances[-$allyL+'x'] ? $l_balances[-$allyL+'x'].balance : 0;
		if (!$balance && !$obalance)
			return;
		const $ratio_L1 = $pow($ratio_powers, $L) / $ratio; // $ratio**($L-1)
		const $debt_ratio = ($L-1)/$L;
		if ($balance) {
			const $delta_XL_balance = $balance * ($ratio_L1 - 1);
			const $new_XL_balance = $balance + $delta_XL_balance;
			$l_balances[$allyL+'x'].balance = $new_XL_balance;
			const $delta_YL_balance = -(($new_XL_balance * $final_P - $balance * $P) * $debt_ratio); // borrowed
			$totals.delta_XL = $totals.delta_XL + $delta_XL_balance;
			$totals.delta_YL = $totals.delta_YL + $delta_YL_balance;
			$totals.XL_denom = $totals.XL_denom + $new_XL_balance * ($L-1);
		}
		if ($obalance) { // e.g. L=-2
			const $delta_YL_obalance = $obalance * (1/$ratio_L1 - 1);
			const $new_YL_obalance = $obalance + $delta_YL_obalance;
			$l_balances[-$allyL+'x'].balance = $new_YL_obalance;
			const $delta_XL_obalance = -(($new_YL_obalance / $final_P - $obalance / $P) * $debt_ratio); // borrowed
			$totals.delta_YL = $totals.delta_YL + $delta_YL_obalance;
			$totals.delta_XL = $totals.delta_XL + $delta_XL_obalance;
			$totals.YL_denom = $totals.YL_denom + $new_YL_obalance * ($L-1);
		}
	});
	return $totals
};



const $swap_by_delta_y = ($balances, $l_balances, $profits, $recent, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $trigger_initial_address, $pool_props) => {
			
	require_cond(!$in_final_P, "no final price please, this is swap by Y");
	
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	const $Lambda = $pool_props.Lambda;

	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;

	let $inverted, $X, $Y, $Xn, $Yn, $X0, $Y0, $a, $b, $in_token, $out_token;
	
	if ($y_in){
		$inverted = false;
		$X = $x;
		$Y = $y;
		$Xn = $xn;
		$Yn = $yn;
		$X0 = $x0;
		$Y0 = $y0;
		$a = $alpha;
		$b = $beta;
		$in_token = 'y';
		$out_token = 'x';
	}
	else{ // x <-> y swap their roles. Uppercase X, Y, and P refer to invertable values
		$inverted = true;
		$X = $y;
		$Y = $x;
		$Xn = $yn;
		$Yn = $xn;
		$X0 = $y0;
		$Y0 = $x0;
		$a = $beta;
		$b = $alpha;
		$in_token = 'x';
		$out_token = 'y';
	}
//	require_cond($delta_Yn > 0 && is_integer($delta_Yn), "bad delta " + $delta_Yn);
	require_cond($delta_Yn > 0, "bad delta " + $delta_Yn);

	if ($Lambda > 1){
		var $underleveraged = $Xn > ceil($X/$Lambda);
	}

	const $final_Yn = $Yn + $delta_Yn;

	let $final_X, $final_Y, $final_Xn;

	if ($Lambda === 1){
		$final_Xn = $get_final_x($X, $Y, $final_Yn, $X0, $Y0, $pool_props, $inverted);
		$final_X = $final_Xn;
		$final_Y = $final_Yn;
	}
	else if (!$underleveraged){ // along X
		const $delta_Y = -($b*$Lambda-1)/$a*$delta_Yn;
		$final_Y = $Y + $delta_Y;
		require_cond($final_Y > 0, "fully leveraged: negative final_Y="+$final_Y);
		$final_X = $get_final_x_along_x($X, $Y, $final_Y, $pool_props, $inverted);
		$final_Xn = $final_X/$Lambda;
	}
	else if ($underleveraged){
		const $delta_Yn_inflection = $Y * (( $Lambda/($Lambda-1) * ($b + ($a * $Lambda - 1) * $Xn/$X) )**($a * $Lambda/($a*$Lambda-1)) - 1) / $Lambda;
		require_cond($delta_Yn_inflection > 0, "negative delta_Yn_inflection="+$delta_Yn_inflection);
		const $inflected = $delta_Yn > $delta_Yn_inflection;
		// along Y until the inflection point
		const $inflection_Yn = $Yn + $delta_Yn_inflection;
		const $final_Yn1 = $inflected ? $inflection_Yn : $final_Yn;
		const $final_Y1 = $final_Yn1 * $Lambda;
		const $final_X1 = $get_final_x_along_y($X, $Y, $final_Y1, $pool_props, $inverted);
		const $delta_X1 = $final_X1 - $X;
		const $delta_Xn1 = -$b/($a*$Lambda-1) * $delta_X1;
		const $final_Xn1 = $Xn + $delta_Xn1;
		require_cond($final_Xn1 > 0, "negative final_Xn1="+$final_Xn1);
		if ($inflected){
			// then, along X
			log('inflected at ', $delta_Yn_inflection);
			const $delta_Yn2 = $final_Yn - $final_Yn1;
			const $delta_Y2 = -($b*$Lambda-1)/$a*$delta_Yn2;
			$final_Y = $final_Y1 + $delta_Y2;
			require_cond($final_Y > 0, "inflected: negative final_Y="+$final_Y);
			$final_X = $get_final_x_along_x($final_X1, $final_Y1, $final_Y, $pool_props, $inverted);
			$final_Xn = $final_X/$Lambda;
			require_cond($final_Xn <= $final_Xn1, "Xn didn't decrease");
		}
		else{
			$final_X = $final_X1;
			$final_Xn = $final_Xn1;
			$final_Y = $final_Y1;
		}
	}
	else
		bounce("???");
	
	$balances.x = $y_in ? $final_X : $final_Y;
	$balances.y = $y_in ? $final_Y : $final_X;
	$balances.xn = $y_in ? $final_Xn : $final_Yn;
	$balances.yn = $y_in ? $final_Yn : $final_Xn;

	const $final_y = $balances.y;
	const $final_x = $balances.x;

	const $p = $alpha/$beta * ($y + $y0) / ($x + $x0); // price of x in terms of y
	const $P = $inverted ? 1/$p : $p; // price of X in terms of Y
	const $final_p = $alpha/$beta * ($final_y + $y0) / ($final_x + $x0);
	const $final_P = $inverted ? 1/$final_p : $final_p;
	require_cond($final_P > $P, "price should have risen but hasn't, old " + $P + ", new " + $final_P);

	// if inverted, XL corresponds to y, YL to x
	const $totals = $update_leveraged_balances($l_balances, $P, $final_P, $inverted);

	const $amount_X_exact = -($final_Xn - $Xn + $totals.delta_XL);
	const $amount_Y_exact = $final_Yn - $Yn + $totals.delta_YL;
	const $amount_Y = ceil($amount_Y_exact);
	if ($received_amount_Y >= 0)
		require_cond($received_amount_Y >= $amount_Y, "expected " + $amount_Y + ", received " + $received_amount_Y);
	require_cond($amount_X_exact >= 0, "to pay " + $amount_X_exact);
	const $change = $received_amount_Y - $amount_Y;

	const $denom = 1 - $totals.XL_denom/$b/($final_X+$X0) - $totals.YL_denom/$a/($final_Y+$Y0);
	log('denom after swap by delta', $denom);
	require_cond($denom >= $singularity_threshold, "too close to the singularity point, denom="+$denom+", need more liquidity in order to swap this amount");

	// arb tax based on price difference
	let $min_P, $max_P, $recent_traded_amount = 0, $recent_paid_tax = 0;
	if ($recent.last_trade && $recent.last_trade.address == $trigger_initial_address && $recent.last_ts >= timestamp() - $trade_merge_period){
		$min_P = min($P, $y_in ? $recent.last_trade.pmin : 1/$recent.last_trade.pmax);
		$max_P = max($final_P, $y_in ? $recent.last_trade.pmax : 1/$recent.last_trade.pmin);
		$recent_traded_amount = $recent.last_trade.amounts[$out_token];
		$recent_paid_tax = $recent.last_trade.paid_taxes[$out_token];
	}
	else{
		$min_P = $P;
		$max_P = $final_P;
	}
	const $arb_profit_in_Y = ($max_P - $min_P) * ($recent_traded_amount + $amount_X_exact) / 2; // in Y
	const $arb_profit_in_X = $arb_profit_in_Y / $min_P;
	const $arb_profit_tax = $arb_profit_in_X * $pool_props.arb_profit_tax - $recent_paid_tax;

	const $swap_fee = $amount_X_exact * $pool_props.swap_fee;
	const $fee = $arb_profit_tax + $swap_fee;

	const $net_amount_X_exact = $amount_X_exact - $fee;
	const $net_amount_X = floor($net_amount_X_exact);
	const $rounding_fee_X = $net_amount_X_exact - $net_amount_X;
	const $rounding_fee_Y = $amount_Y - $amount_Y_exact;
	const $total_fee = $fee + $rounding_fee_X;

	if ($min_amount_out)
		require_cond($net_amount_X >= $min_amount_out, "output amount " + $net_amount_X + " would be less than the expected minimum " + $min_amount_out);

	// include rounding fees
	const $fees = {
		X: $total_fee,
		Y: $rounding_fee_Y,
	};

	// add the fee to the pool without trading and affecting the price (Lambda>1) or to a separate profit accumulator (Lambda=1)
	$add_net_balance_without_changing_price($balances, $profits, $out_token, $fees.X, $Lambda);
	$add_net_balance_without_changing_price($balances, $profits, $in_token, $fees.Y, $Lambda);
	log({fees: $fees, profits: $profits});

	$update_recent_data($recent, $p, $final_p, $trigger_initial_address, $out_token, $amount_X_exact, $arb_profit_tax, $pool_props.period_length);

	const $event = JSON.stringify({
		type: 'swap',
		direction: $y_in ? 'y2x' : 'x2y',
		in: $amount_Y,
		out: $net_amount_X,
		swap_fee: $swap_fee,
		arb_profit_tax: $arb_profit_tax,
		total_fee: $total_fee,
	});

	return {
		net_amount_X: $net_amount_X,
		amount_Y: $amount_Y,
		swap_fee: $swap_fee,
		arb_profit_tax: $arb_profit_tax,
		total_fee: $total_fee,
		fees: $fees,
		change: $change,
		initial_price: $P,
		final_price: $final_P,
		event: $event,
	}
};



const $swap_by_final_p = ($balances, $l_balances, $profits, $recent, $x0, $y0, $y_in, $in_delta_Yn, $final_P, $received_amount_Y, $min_amount_out, $trigger_initial_address, $pool_props) => {
			
	require_cond(!$in_delta_Yn, "no delta Yn please, this is swap by P");
	
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	const $Lambda = $pool_props.Lambda;

	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;
	
	let $inverted, $X, $Y, $Xn, $Yn, $X0, $Y0, $a, $b, $in_token, $out_token;

	if ($y_in) {
		$inverted = false;
		$X = $x;
		$Y = $y;
		$Xn = $xn;
		$Yn = $yn;
		$X0 = $x0;
		$Y0 = $y0;
		$a = $alpha;
		$b = $beta;
		$in_token = 'y';
		$out_token = 'x';
	}
	else{ // x <-> y swap their roles. Uppercase X, Y, and P refer to invertable values
		$inverted = true;
		$X = $y;
		$Y = $x;
		$Xn = $yn;
		$Yn = $xn;
		$X0 = $y0;
		$Y0 = $x0;
		$a = $beta;
		$b = $alpha;
		$in_token = 'x';
		$out_token = 'y';
	}
	const $P = $a/$b * ($Y + $Y0) / ($X + $X0); // price of X in terms of Y
	require_cond($final_P > $P, "price should increase, current " + $P + ", target " + $final_P);

	if ($Lambda > 1){
		var $underleveraged = $Xn > ceil($X/$Lambda);
	}

	let $final_X, $final_Y, $final_Xn, $final_Yn;

	if ($Lambda == 1) {
		const $final = $get_final_xy($X, $Y, $P, $final_P, $X0, $Y0, $pool_props, $inverted);
		$final_X = $final.X;
		$final_Y = $final.Y;
		$final_Xn = $final_X;
		$final_Yn = $final_Y;
	}
	else if (!$underleveraged){ // along X
		const $final = $get_final_xy_along_x($X, $Y, $P, $final_P, $pool_props, $inverted);
		$final_X = $final.X;
		$final_Y = $final.Y;
		$final_Xn = $final_X/$Lambda;
		const $delta_Y = $final_Y - $Y;
		const $delta_Yn = -$a/($b*$Lambda-1)*$delta_Y;
		$final_Yn = $Yn + $delta_Yn;
		require_cond($final_Yn > 0, "fully leveraged: negative final_Yn="+$final_Yn);
	}
	else if ($underleveraged){
		const $inflection_P = $P * ( $Lambda/($Lambda-1) * ($b + ($a * $Lambda - 1) * $Xn/$X) )**(1/($a*$Lambda-1));
		require_cond($inflection_P > 0, "negative inflection_P="+$inflection_P);
		const $inflected = $final_P > $inflection_P;
		// along Y until the inflection point
		const $final_P1 = $inflected ? $inflection_P : $final_P;
		const $final1 = $get_final_xy_along_y($X, $Y, $P, $final_P1, $pool_props, $inverted);
		const $final_X1 = $final1.X;
		const $final_Y1 = $final1.Y;
		const $final_Yn1 = $final_Y1 / $Lambda;
		const $delta_X1 = $final_X1 - $X;
		const $delta_Xn1 = -$b/($a*$Lambda-1) * $delta_X1;
		const $final_Xn1 = $Xn + $delta_Xn1;
		require_cond($final_Xn1 > 0, "negative final_Xn1="+$final_Xn1);
		if ($inflected){
			// then, along X
			log('inflected at price', $inflection_P);
			const $final = $get_final_xy_along_x($final_X1, $final_Y1, $final_P1, $final_P, $pool_props, $inverted);
			$final_X = $final.X;
			$final_Y = $final.Y;
			$final_Xn = $final_X/$Lambda;
			const $delta_Y2 = $final_Y - $final_Y1;
			const $delta_Yn2 = -$a/($b*$Lambda-1)*$delta_Y2;
			$final_Yn = $final_Yn1 + $delta_Yn2;
			require_cond($final_Xn > 0, "negative final_Xn="+$final_Xn);
			require_cond($final_Xn <= $final_Xn1, "Xn didn't decrease");
		}
		else{
			$final_X = $final_X1;
			$final_Y = $final_Y1;
			$final_Xn = $final_Xn1;
			$final_Yn = $final_Yn1;
		}
	}
	else
		bounce("???");
	
	$balances.x = $y_in ? $final_X : $final_Y;
	$balances.y = $y_in ? $final_Y : $final_X;
	$balances.xn = $y_in ? $final_Xn : $final_Yn;
	$balances.yn = $y_in ? $final_Yn : $final_Xn;
//	log("balances after swap", $balances);

	// if inverted, XL corresponds to y, YL to x
	const $totals = $update_leveraged_balances($l_balances, $P, $final_P, $inverted);

	const $amount_X_exact = -($final_Xn - $Xn + $totals.delta_XL);
	const $amount_Y_exact = $final_Yn - $Yn + $totals.delta_YL;
	const $amount_Y = ceil($amount_Y_exact);
	if ($received_amount_Y >= 0)
		require_cond($received_amount_Y >= $amount_Y, "expected " + $amount_Y + ", received " + $received_amount_Y);
	require_cond($amount_X_exact >= 0, "to pay " + $amount_X_exact);
	const $change = $received_amount_Y - $amount_Y;

	const $denom = 1 - $totals.XL_denom/$b/($final_X+$X0) - $totals.YL_denom/$a/($final_Y+$Y0);
//	log('denom after swap to price:', $denom);
	require_cond($denom >= $singularity_threshold, "too close to the singularity point, denom="+$denom+", need more liquidity in order to swap this amount");

	// arb tax based on price difference
	let $min_P, $max_P, $recent_traded_amount = 0, $recent_paid_tax = 0;
	if ($recent.last_trade && $recent.last_trade.address == $trigger_initial_address && $recent.last_ts >= timestamp() - $trade_merge_period){
		$min_P = min($P, $y_in ? $recent.last_trade.pmin : 1/$recent.last_trade.pmax);
		$max_P = max($final_P, $y_in ? $recent.last_trade.pmax : 1/$recent.last_trade.pmin);
		$recent_traded_amount = $recent.last_trade.amounts[$out_token];
		$recent_paid_tax = $recent.last_trade.paid_taxes[$out_token];
	}
	else{
		$min_P = $P;
		$max_P = $final_P;
	}
	const $arb_profit_in_Y = ($max_P - $min_P) * ($recent_traded_amount + $amount_X_exact) / 2; // in Y
	const $arb_profit_in_X = $arb_profit_in_Y / $min_P;
	const $arb_profit_tax = $arb_profit_in_X * $pool_props.arb_profit_tax - $recent_paid_tax;

	const $swap_fee = $amount_X_exact * $pool_props.swap_fee;
	const $fee = $arb_profit_tax + $swap_fee;
	
	const $net_amount_X_exact = $amount_X_exact - $fee;
	const $net_amount_X = floor($net_amount_X_exact);
	const $rounding_fee_X = $net_amount_X_exact - $net_amount_X;
	const $rounding_fee_Y = $amount_Y - $amount_Y_exact;
	const $total_fee = $fee + $rounding_fee_X;

	if ($min_amount_out)
		require_cond($net_amount_X >= $min_amount_out, "output amount " + $net_amount_X + " would be less than the expected minimum " + $min_amount_out);
	
	// include rounding fees
	const $fees = {
		X: $total_fee,
		Y: $rounding_fee_Y,
	};

	// add the fee to the pool without trading and affecting the price (Lambda>1) or to a separate profit accumulator (Lambda=1)
	$add_net_balance_without_changing_price($balances, $profits, $out_token, $fees.X, $Lambda);
	$add_net_balance_without_changing_price($balances, $profits, $in_token, $fees.Y, $Lambda);
//	log("balances after adding the fees", $balances);

	$update_recent_data($recent, $inverted ? 1/$P : $P, $inverted ? 1/$final_P : $final_P, $trigger_initial_address, $out_token, $amount_X_exact, $arb_profit_tax, $pool_props.period_length);

	const $event = JSON.stringify({
		type: 'swap',
		direction: $y_in ? 'y2x' : 'x2y',
		in: $amount_Y,
		out: $net_amount_X,
		swap_fee: $swap_fee,
		arb_profit_tax: $arb_profit_tax,
		total_fee: $total_fee,
	});

	return {
		net_amount_X: $net_amount_X,
		amount_Y: $amount_Y,
		swap_fee: $swap_fee,
		arb_profit_tax: $arb_profit_tax,
		total_fee: $total_fee,
		fees: $fees,
		change: $change,
		initial_price: $P,
		final_price: $final_P,
		event: $event,
	}
};



const $swap = ($balances, $l_balances, $profits, $recent, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $trigger_initial_address, $pool_props) => {
	if ($delta_Yn)
		return $swap_by_delta_y($balances, $l_balances, $profits, $recent, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $trigger_initial_address, $pool_props);
	else if ($in_final_P)
		return $swap_by_final_p($balances, $l_balances, $profits, $recent, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $trigger_initial_address, $pool_props);
	else
		throw Error("unclear swap type")
}


const $buy_shares = ($s, $balances, $profits, $recent, $x0, $y0, $received_amount_x, $received_amount_y, $pool_props) => {
			
	const $Lambda = $pool_props.Lambda;
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	
//	$get_shares = ($x_balance, $y_balance) => round($x_balance**$alpha * $y_balance**$beta);
	const $get_shares = ($x_balance, $y_balance) => round(($x_balance/$y_balance)**$alpha * $y_balance);

	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;

	$recent.last_ts = timestamp();
	
	if (!$s){
		require_cond($received_amount_x > 0 && $received_amount_y > 0, "send both assets for the first issue");
		let $shares_amount;
		const $mid_price = $pool_props.mid_price;
		if ($mid_price){
			// first issue must be at mid price
			require_cond($received_amount_y == round($mid_price * $received_amount_x), "first issue must be at mid price "+$mid_price);
			const $gamma = $pool_props.gamma;
			$shares_amount = round($received_amount_x * $pool_props.mid_price_beta * $gamma / ($gamma - 1));
		}
		else{
			// first issue determines the price
			$shares_amount = $get_shares($received_amount_x, $received_amount_y);
		}
		$balances.xn = $balances.xn + $received_amount_x;
		$balances.yn = $balances.yn + $received_amount_y;
		$balances.x = $balances.x + ($received_amount_x * $Lambda);
		$balances.y = $balances.y + ($received_amount_y * $Lambda);
		
		const $event = JSON.stringify({
			type: 'add',
			x: $received_amount_x,
			y: $received_amount_y,
			shares: $shares_amount,
		});
	
		return {
			shares_amount: $shares_amount,
			coef: 1,
			change_x: 0,
			change_y: 0,
			event: $event,
		};
	}

	const $p = $alpha / $beta * ($y + $y0) / ($x + $x0);
	let $shares1 = 0, $delta_yn1 = 0, $delta_y1 = 0, $delta_xn1 = 0, $delta_x1 = 0;
	if ($Lambda > 1 && $recent.prev){
		const $target_xn = ($x/$Lambda);
		if ($xn > ceil($target_xn)){ // x is underleveraged
		//	require_cond($recent.prev, "too early, prev price not known yet");
			// use the worst (for the user) price that was seen recently
			const $share_price_in_y = ($yn + max($recent.current.pmax, $recent.prev.pmax) * $xn) / $s;
			const $max_delta_yn = ($xn/$target_xn-1)*$yn;
			$delta_yn1 = min($max_delta_yn, $received_amount_y);
			$delta_y1 = $delta_yn1 * $Lambda;
			$delta_x1 = $x * $delta_yn1/$yn; // proportional
			$shares1 = $delta_yn1/$share_price_in_y;
		}
		else{
			const $target_yn = $y/$Lambda;
			if ($yn > ceil($target_yn)){ // y is underleveraged
			//	require_cond($recent.prev, "too early, prev price not known yet");
				// use the worst (for the user) price that was seen recently
				const $share_price_in_x = ($xn + 1/min($recent.current.pmin, $recent.prev.pmin) * $yn) / $s;
				const $max_delta_xn = ($yn/$target_yn-1)*$xn;
				$delta_xn1 = min($max_delta_xn, $received_amount_x);
				$delta_x1 = $delta_xn1 * $Lambda;
				$delta_y1 = $y * $delta_xn1/$xn; // proportional
				$shares1 = $delta_xn1/$share_price_in_x;
				log({delta_xn1:$delta_xn1, shares1:$shares1, share_price_in_x:$share_price_in_x, p:$p, recent:$recent});
			}
		}
		$balances.xn = $balances.xn + $delta_xn1;
		$balances.yn = $balances.yn + $delta_yn1;
		$balances.x = $balances.x + $delta_x1;
		$balances.y = $balances.y + $delta_y1;
	}
	else{
		$delta_xn1 = 0;
		$delta_yn1 = 0;
		$shares1 = 0;
	}
	const $remaining = {
		x: $received_amount_x - $delta_xn1,
		y: $received_amount_y - $delta_yn1,
	};

	const $y_to_x = ($yn+$delta_yn1)/($xn+$delta_xn1);
//	$y_to_x = $balances.yn/$balances.xn;
	let $symmetric_moved_profit_shares = 0, $moved_profit_shares = 0

	if ($profits.x || $profits.y){
		require_cond($Lambda == 1, "have profits while Lambda is " + $Lambda);
		require_cond($profits.x >= 0 && $profits.y >= 0, "negative profits?"); // should never happen

		// latest prices, not recent min/max
		const $share_price_in_y = ($yn + $p * $xn) / $s;
		const $share_price_in_x = ($xn + 1/$p * $yn) / $s;

		// move proportional amounts of profit from both x and y. The shares to be issued for the moved profit belong to the pool and will not be actually issued
		let $delta_profit_x, $delta_profit_y;
		const $profits_proportional_y = $y_to_x * $profits.x;
		if ($profits.y > $profits_proportional_y){
			$delta_profit_x = $profits.x;
			$delta_profit_y = $profits_proportional_y;
			$symmetric_moved_profit_shares = $delta_profit_x/$xn * $s;
		}
		else{
			const $profits_proportional_x = $profits.y / $y_to_x;
			require_cond($profits_proportional_x <= $profits.x, "profits x " + $profits.x + ", proportional " + $profits_proportional_x);
			$delta_profit_x = $profits_proportional_x;
			$delta_profit_y = $profits.y;
			$symmetric_moved_profit_shares = $delta_profit_y/$yn * $s;
		}
		$profits.x = $profits.x - $delta_profit_x;
		$profits.y = $profits.y - $delta_profit_y;
		$balances.xn = $balances.xn + $delta_profit_x;
		$balances.yn = $balances.yn + $delta_profit_y;
		$balances.x = $balances.x + $delta_profit_x;
		$balances.y = $balances.y + $delta_profit_y;
		log('after proportional profits: delta_profit_x', $delta_profit_x, 'delta_profit_y', $delta_profit_y, 'remaining profits', $profits, 'symmetric_moved_profit_shares', $symmetric_moved_profit_shares);

		// calc the shares to be issued for moving the one-sided profits to the pool, these shares belong to the pool and will not be actually issued. The user contributes the other side and gets the shares cheaper than their fair price because profits are not included in the price calculation.
		const $moved_profit_x = min($profits.x, $remaining.y / $y_to_x);
		const $moved_profit_y = min($profits.y, $remaining.x * $y_to_x);

		$moved_profit_shares = $moved_profit_x/$share_price_in_x + $moved_profit_y/$share_price_in_y + $symmetric_moved_profit_shares;
		log('share_price_in_x', $share_price_in_x, 'share_price_in_y', $share_price_in_y, 'moved_profit_shares', $moved_profit_shares, 'moved_profit_x', $moved_profit_x, 'moved_profit_y', $moved_profit_y);
		
		$profits.x = $profits.x - $moved_profit_x;
		$profits.y = $profits.y - $moved_profit_y;
		
		$remaining.x = $remaining.x + $moved_profit_x;
		$remaining.y = $remaining.y + $moved_profit_y;
	}

	// part 2: proportional buying
	log('before proportional buying: remaining', $remaining, 'y_to_x', $y_to_x);
	let $exact_change_x = 0, $exact_change_y = 0, $proportional_delta_xn, $proportional_delta_yn, $shares_proportional;
	$remaining_proportional_y = $y_to_x * $remaining.x;
	if ($remaining.y > $remaining_proportional_y){ // excessive y
		$proportional_delta_xn = $remaining.x;
		$proportional_delta_yn = $remaining_proportional_y;
		$exact_change_x = 0;
		$exact_change_y = $remaining.y - $remaining_proportional_y;
		log({proportional_delta_yn:$proportional_delta_yn, exact_change_y:$exact_change_y, change_y_exact: $remaining.y - $remaining_proportional_y, remaining_y:$remaining.y, remaining_proportional_y:$remaining_proportional_y});
		$shares_proportional = $remaining.x / ($xn + $delta_xn1) * ($s + $shares1);
	}
	else{ // excessive x
		const $remaining_proportional_x = $remaining.y / $y_to_x;
		$proportional_delta_xn = $remaining_proportional_x;
		$proportional_delta_yn = $remaining.y;
		$exact_change_x = $remaining.x - $remaining_proportional_x;
		log({proportional_delta_xn:$proportional_delta_xn, exact_change_x:$exact_change_x, remaining_x:$remaining.x, remaining_proportional_x:$remaining_proportional_x});
		require_cond($exact_change_x >= 0, "received x " + $remaining.x + ", proportional " + $remaining_proportional_x);
		$exact_change_y = 0;
		$shares_proportional = $remaining.y / ($yn + $delta_yn1) * ($s + $shares1);
	}

	const $gross_shares_amount = $shares1 + $symmetric_moved_profit_shares + $shares_proportional;
	const $shares_amount = floor($gross_shares_amount - $moved_profit_shares);
	const $coef = $Lambda == 1 ? ($s + $gross_shares_amount) / ($s + $shares_amount) : 1;
	log({shares_proportional:$shares_proportional, moved_profit_shares:$moved_profit_shares, shares_amount:$shares_amount});

	$balances.xn = $balances.xn + $proportional_delta_xn;
	$balances.yn = $balances.yn + $proportional_delta_yn;
	$balances.x = $balances.x + $proportional_delta_xn * $Lambda;
	$balances.y = $balances.y + $proportional_delta_yn * $Lambda;

	const $change_x = floor($exact_change_x);
	const $change_y = floor($exact_change_y);
	const $rounding_fee_x = $exact_change_x - $change_x;
	const $rounding_fee_y = $exact_change_y - $change_y;

	$add_net_balance_without_changing_price($balances, $profits, 'x', $rounding_fee_x, $Lambda);
	$add_net_balance_without_changing_price($balances, $profits, 'y', $rounding_fee_y, $Lambda);

	const $event = JSON.stringify({
		type: 'add',
		x: $received_amount_x - $change_x,
		y: $received_amount_y - $change_y,
		shares: $shares_amount,
	});
	
	return {
		shares_amount: $shares_amount,
		coef: $coef,
		change_x: $change_x,
		change_y: $change_y,
		event: $event,
	}

};


const $redeem_shares = ($s, $balances, $l_balances, $profits, $recent, $x0, $y0, $received_shares_amount, $asset, $pool_props) => {
	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;
	const $exit_fee = ($received_shares_amount < $s) ? $pool_props.exit_fee : 0; // 0 fee for the last LP
	const $net_of_exit_fee = 1 - $exit_fee;
	const $x_asset = $pool_props.x_asset;
	const $y_asset = $pool_props.y_asset;
	const $Lambda = $pool_props.Lambda;
	const $alpha = $pool_props.alpha;
//	$beta = $pool_props.beta;
	let $xn_amount1, $yn_amount1, $remaining_received_shares, $one_sided_x_fee = 0, $one_sided_y_fee = 0;
	if ($asset){ // one-sided redemption first, then proportional
		require_cond($asset == $x_asset || $asset == $y_asset, "wrong preferred asset");
		require_cond($Lambda > 1, "only proportional withdrawals allowed");
		const $asset_label = $asset == $x_asset ? 'x' : 'y';
		const $net_balance = $asset == $x_asset ? $xn : $yn;
		const $effective_balance = $asset == $x_asset ? $x : $y;
		const $target_net_balance = $effective_balance / $Lambda;
	//	require_cond($target_net_balance < $net_balance, "the preferred asset is already fully leveraged");
		const $excess_net_balance = max($net_balance - $target_net_balance, 0);
	//	$p = $alpha/$beta * $y / $x;
		require_cond($recent.prev, "too early, prev price not known yet");
		// use the worst (for the user) price that was seen recently
		const $share_price_in_asset = (($asset_label == 'y') ? ($yn + min($recent.current.pmin, $recent.prev.pmin) * $xn) / $s : ($xn + 1/max($recent.current.pmax, $recent.prev.pmax) * $yn) / $s) * $net_of_exit_fee;
		const $max_asset = $received_shares_amount * $share_price_in_asset;
		const $one_sided_amount = min($max_asset, $excess_net_balance);
		const $one_sided_fee = $one_sided_amount / $net_of_exit_fee * $exit_fee;
		if ($asset_label == 'y'){
			$yn_amount1 = $one_sided_amount;
			$xn_amount1 = 0;
			$one_sided_y_fee = $one_sided_fee;
		}
		else{
			$xn_amount1 = $one_sided_amount;
			$yn_amount1 = 0;
			$one_sided_x_fee = $one_sided_fee;
		}
		$remaining_received_shares = max($received_shares_amount - ($one_sided_amount / $share_price_in_asset), 0);
	}
	else{
		$remaining_received_shares = $received_shares_amount;
		$xn_amount1 = 0;
		$yn_amount1 = 0;
	}
	const $share_of_shares = $remaining_received_shares / $s;
	const $remaining_share_of_shares = 1 - $share_of_shares;
	const $remaining_share_of_assets = $remaining_share_of_shares;
	const $share_of_assets = 1 - $remaining_share_of_assets;
	const $x_amount = $share_of_assets * $x * $net_of_exit_fee;
	const $y_amount = $share_of_assets * $y * $net_of_exit_fee;
	const $xn_amount = $share_of_assets * ($xn - $xn_amount1) * $net_of_exit_fee + $xn_amount1;
	const $yn_amount = $share_of_assets * ($yn - $yn_amount1) * $net_of_exit_fee + $yn_amount1;
//	if ($asset)
//		log({xn_amount1:$xn_amount1, yn_amount1:$yn_amount1, shares_for_excess: $one_sided_amount / $share_price_in_asset, remaining_received_shares:$remaining_received_shares, xn_amount:$xn_amount, yn_amount:$yn_amount, asset_label:$asset_label, share_price_in_asset:$share_price_in_asset, max_asset: $max_asset, excess_net_balance:$excess_net_balance, recent:$recent, pmin:min($recent.current.pmin, $recent.prev.pmin), pmax:max($recent.current.pmax, $recent.prev.pmax)});
	log({ remaining_received_shares:$remaining_received_shares, xn_amount:$xn_amount, yn_amount:$yn_amount, x_amount:$x_amount, y_amount:$y_amount});

	log('balances before', $balances);
	$balances.x = $balances.x - $x_amount;
	$balances.y = $balances.y - $y_amount;
	$balances.xn = $balances.xn - $xn_amount;
	$balances.yn = $balances.yn - $yn_amount;
	log('balances after', $balances);

	const $coef = $Lambda == 1 && $received_shares_amount < $s ? ($s - $received_shares_amount * $net_of_exit_fee) / ($s - $received_shares_amount) : 1;

	const $new_x0 = $x0 * ($s-$received_shares_amount)/$s;
	const $new_y0 = $y0 * ($s-$received_shares_amount)/$s;
	const $denom = 1 - $get_utilization_ratio($balances, $l_balances, $new_x0, $new_y0, $alpha);
	require_cond($denom >= $singularity_threshold, "redemption amount too large, it would bring us too close to the singularity point, denom="+$denom);

	const $int_xn_amount = floor($xn_amount);
	const $int_yn_amount = floor($yn_amount);
	const $rounding_fee_x = $xn_amount - $int_xn_amount;
	const $rounding_fee_y = $yn_amount - $int_yn_amount;

	$add_net_balance_without_changing_price($balances, $profits, 'x', $rounding_fee_x, $Lambda);
	$add_net_balance_without_changing_price($balances, $profits, 'y', $rounding_fee_y, $Lambda);
	log('balances after rounding fees', $balances);

	$recent.last_ts = timestamp();

	const $event = JSON.stringify({
		type: 'remove',
		shares: $received_shares_amount,
		x: $int_xn_amount,
		y: $int_yn_amount,
		x_fee: $share_of_assets * ($xn - $xn_amount1) * $exit_fee + $one_sided_x_fee,
		y_fee: $share_of_assets * ($yn - $yn_amount1) * $exit_fee + $one_sided_y_fee,
	});
	
	return {
		xn_amount: $int_xn_amount,
		yn_amount: $int_yn_amount,
		coef: $coef,
		event: $event,
	}
};


const $update_other_l_balances_and_get_sums = ($l_balances, $P, $final_P, $Leverage, $inverted) => {
	const $ratio = $final_P/$P;
	const $ratio_powers = $precompute($ratio);
	let $sums = {
		initial: 0,
		final: 0,
		delta_XL: 0,
		XL_denom: 0,
		YL_denom: 0,
		PL1_ratio: $pow($ratio_powers, $Leverage) / $ratio, // $ratio**($Leverage-1)
	};
	$get_leverages().forEach($L => {
		const $allyL = $inverted ? -$L : $L;
		const $balance = $l_balances[$allyL+'x'] ? $l_balances[$allyL+'x'].balance : 0;
		const $obalance = $l_balances[-$allyL+'x'] ? $l_balances[-$allyL+'x'].balance : 0;
		if (!$balance && !$obalance)
			return;
		const $ratio_L1 = $pow($ratio_powers, $L) / $ratio; // $ratio**($L-1)
		if ($balance){
			$sums.initial = $sums.initial + $balance * ($L-1)/$L;
			if ($L != $Leverage){
				const $new_balance = $balance * $ratio_L1;
				$l_balances[$allyL+'x'].balance = $new_balance;
				$sums.final = $sums.final + $new_balance * ($L-1)/$L;
				$sums.XL_denom = $sums.XL_denom + $new_balance * ($L-1);
				$sums.delta_XL = $sums.delta_XL + $new_balance - $balance;
			}
		}
		if ($obalance){
			$sums.initial = $sums.initial - $obalance/$P;
			const $new_obalance = $obalance / $ratio_L1;
			$l_balances[-$allyL+'x'].balance = $new_obalance;
			$sums.final = $sums.final - $new_obalance/$final_P;
			$sums.YL_denom = $sums.YL_denom + $new_obalance * ($L-1);
			$sums.delta_XL = $sums.delta_XL - ($new_obalance / $final_P - $obalance / $P) * ($L-1)/$L;
		}
	});
	return $sums
};



const $move_unleveraged = ($pool, $l_balances, $X0, $Y0, $dXn, $Leverage, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha;
	const $b = 1 - $a;
	const $L_key = ($inverted ? -$Leverage : $Leverage) + 'x';
	const $l_bal_direction = $dXn < 0 ? "grow" : "fall";

	const $Xt = $pool.X + $X0;
	const $P = $a/$b * ($pool.Y + $Y0) / $Xt;
	const $final_Xn = $pool.Xn + $dXn;
	require_cond($final_Xn > 0, "unleveraged: final_Xn=" + $final_Xn);
	const $final_X = $final_Xn;
	const $final_Xt = $final_X + $X0;
	const $final_Y = $get_final_y($pool.X, $pool.Y, $final_X, $X0, $Y0, $pool_props, $inverted);
	const $delta_Y = $final_Y - $pool.Y;
	const $delta_Yn = $delta_Y;
	const $final_Yn = $pool.Yn + $delta_Yn;
	require_cond($final_Yn > 0, "unleveraged: final_Yn=" + $final_Yn);
	const $final_P = $a/$b * ($final_Y + $Y0) / $final_Xt;

//	log('l balances before', $l_balances);
	const $sums = $update_other_l_balances_and_get_sums($l_balances, $P, $final_P, $Leverage, $inverted);
//	log('sums', $sums);
//	log('l balances after', $l_balances);

	// update the final balance of our L-pool
	const $b1 = $sums.initial + $b/($b-1)*$Xt;
	const $new_l_balance = $Leverage/($Leverage-1) * ( -$sums.final - $b/($b-1)*$final_Xt + $b1 * ($final_Xt/$Xt)**(1/$b) );
//	log('new_l_balance', $new_l_balance);
//	require_cond($new_l_balance >= 0, "unleveraged: new_l_balance=" + $new_l_balance);
	const $delta_l_balance = $new_l_balance - $l_balances[$L_key].balance;
//	log('delta_l_balance', $delta_l_balance);
	require_cond($delta_l_balance * $dXn < 0, "unleveraged l-bal should "+$l_bal_direction+", got new " + $new_l_balance + ", delta " + $delta_l_balance);
	$l_balances[$L_key].balance = $new_l_balance;
//	log('l balances after 2', $l_balances);

	$pool.X = $final_X;
	$pool.Y = $final_Y;
	$pool.Xn = $final_Xn;
	$pool.Yn = $final_Yn;
	$pool.delta_XL = $pool.delta_XL + $sums.delta_XL;
	$pool.XL_denom = $sums.XL_denom + $new_l_balance * ($Leverage-1);
	$pool.YL_denom = $sums.YL_denom;
	$pool.PL1_ratio = $pool.PL1_ratio * $sums.PL1_ratio;
};


const $move_along_X = ($pool, $l_balances, $dXn, $Leverage, $pool_props, $inverted) => {
	const $Lambda = $pool_props.Lambda;
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha;
	const $b = 1 - $a;
	const $L_key = ($inverted ? -$Leverage : $Leverage) + 'x';
	const $l_bal_direction = $dXn < 0 ? "grow" : "fall";

	const $P = $a/$b * $pool.Y / $pool.X;
	const $final_Xn = $pool.Xn + $dXn;
	require_cond($final_Xn > 0, "along X: final_Xn=" + $final_Xn);
	const $final_X = $Lambda * $final_Xn;
	const $final_Y = $get_final_y_along_x($pool.X, $pool.Y, $final_X, $pool_props, $inverted);
	const $delta_Y = $final_Y - $pool.Y;
	const $delta_Yn = -$a/($b*$Lambda-1)*$delta_Y;
	const $final_Yn = $pool.Yn + $delta_Yn;
	require_cond($final_Yn > 0, "along X: final_Yn=" + $final_Yn);
	const $final_P = $a/$b * $final_Y / $final_X;

	const $sums = $update_other_l_balances_and_get_sums($l_balances, $P, $final_P, $Leverage, $inverted);

	// update the final balance of our L-pool
	const $b1 = $sums.initial + $b/($b*$Lambda-1)*$pool.X;
	const $new_l_balance = $Leverage/($Leverage-1) * ( -$sums.final - $b/($b*$Lambda-1)*$final_X + $b1 * ($final_X/$pool.X)**(1/$b/$Lambda) );
//	require_cond($new_l_balance >= 0, "along X: new_l_balance=" + $new_l_balance);
	const $delta_l_balance = $new_l_balance - $l_balances[$L_key].balance;
	require_cond($delta_l_balance * $dXn < 0, "along x l-bal should "+$l_bal_direction+", got new " + $new_l_balance + ", delta " + $delta_l_balance);
	$l_balances[$L_key].balance = $new_l_balance;

	$pool.X = $final_X;
	$pool.Y = $final_Y;
	$pool.Xn = $final_Xn;
	$pool.Yn = $final_Yn;
	$pool.delta_XL = $pool.delta_XL + $sums.delta_XL;
	$pool.XL_denom = $sums.XL_denom + $new_l_balance * ($Leverage-1);
	$pool.YL_denom = $sums.YL_denom;
	$pool.PL1_ratio = $pool.PL1_ratio * $sums.PL1_ratio;
};

const $move_along_Y = ($pool, $l_balances, $dXn, $Leverage, $pool_props, $inverted) => {
	const $Lambda = $pool_props.Lambda;
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha;
	const $b = 1 - $a;
	const $L_key = ($inverted ? -$Leverage : $Leverage) + 'x';
	const $l_bal_direction = $dXn < 0 ? "grow" : "fall";

	const $P = $a/$b * $pool.Y / $pool.X;
	const $final_Xn = $pool.Xn + $dXn;
	require_cond($final_Xn > 0, "along Y: final_Xn=" + $final_Xn);
	const $delta_X = -($a*$Lambda-1)/$b * $dXn;
	const $final_X = $pool.X + $delta_X;
	require_cond($final_X > 0, "along Y: final_X=" + $final_X);
	const $final_Y = $get_final_y_along_y($pool.X, $pool.Y, $final_X, $pool_props, $inverted);
	const $final_Yn = $final_Y/$Lambda;
	const $final_P = $a/$b * $final_Y / $final_X;

	const $sums = $update_other_l_balances_and_get_sums($l_balances, $P, $final_P, $Leverage, $inverted);

	// update the final balance of our L-pool
	const $b2 = $sums.initial - $b/$a/$Lambda*$pool.X;
	const $new_l_balance = $Leverage/($Leverage-1) * ( -$sums.final + $b/$a/$Lambda*$final_X + $b2 * ($final_X/$pool.X)**(-1/($a*$Lambda-1)) );
//	require_cond($new_l_balance >= 0, "along Y: new_l_balance=" + $new_l_balance);
	const $delta_l_balance = $new_l_balance - $l_balances[$L_key].balance;
	require_cond($delta_l_balance * $dXn < 0, "along y l-bal should "+$l_bal_direction+", got new " + $new_l_balance + ", delta " + $delta_l_balance);
	$l_balances[$L_key].balance = $new_l_balance;

	$pool.X = $final_X;
	$pool.Y = $final_Y;
	$pool.Xn = $final_Xn;
	$pool.Yn = $final_Yn;
	$pool.delta_XL = $pool.delta_XL + $sums.delta_XL;
	$pool.XL_denom = $sums.XL_denom + $new_l_balance * ($Leverage-1);
	$pool.YL_denom = $sums.YL_denom;
	$pool.PL1_ratio = $pool.PL1_ratio * $sums.PL1_ratio;
};



// delta_Xn < 0: buy L-tokens
// delta_Xn > 0: sell L-tokens
const $trade_l_shares = ($balances, $l_balances, $profits, $recent, $x0, $y0, $Leverage, $asset, $delta_Xn, $entry_price, $trigger_initial_address, $pool_props) => {
//	require_cond(is_integer($delta_Xn), "delta must be int");
	require_cond($asset == $pool_props.x_asset || $asset == $pool_props.y_asset, "wrong asset");
	require_cond($Leverage == 2 || $Leverage == 5 || $Leverage == 10 || $Leverage == 20 || $Leverage == 50 || $Leverage == 100, "bad L");
	const $Lambda = $pool_props.Lambda;

	let $inverted, $X, $Y, $Xn, $Yn, $X0, $Y0, $a, $b, $token;

	if ($asset == $pool_props.x_asset){
		$inverted = false;
		$X = $balances.x;
		$Y = $balances.y;
		$Xn = $balances.xn;
		$Yn = $balances.yn;
		$X0 = $x0;
		$Y0 = $y0;
		$a = $pool_props.alpha;
		$b = $pool_props.beta;
		$token = 'x';
	}
	else{ // x <-> y swap their roles. Uppercase X, Y, and P refer to invertable values
		$inverted = true;
		$X = $balances.y;
		$Y = $balances.x;
		$Xn = $balances.yn;
		$Yn = $balances.xn;
		$X0 = $y0;
		$Y0 = $x0;
		$a = $pool_props.beta;
		$b = $pool_props.alpha;
		$token = 'y';
	}
	const $direct = !$inverted;
	require_cond($Xn + $delta_Xn > 0, "Xn balance would be negative");
	const $L_key = ($inverted ? -$Leverage : $Leverage) + 'x';
	let $pool = {X: $X, Y: $Y, Xn: $Xn, Yn: $Yn, delta_XL: 0, PL1_ratio: 1};


	if (!$l_balances[$L_key])
		$l_balances[$L_key] = { balance: 0, supply: 0 };
	const $initial_l_balance = $l_balances[$L_key].balance;
	const $initial_shares = $l_balances[$L_key].supply;
	// $initial_l_balance might be non-zero after full redemption of all l-shares -- due to rounding of share amounts
//	require_cond(!$initial_l_balance == !$initial_shares, "l-balance "+$initial_l_balance+" while shares "+$initial_shares);
	const $initial_P = $a/$b * ($pool.Y + $Y0) / ($pool.X + $X0);

	if ($Lambda == 1)
		$move_unleveraged($pool, $l_balances, $X0, $Y0, $delta_Xn, $Leverage, $pool_props, $inverted);
	else {
		const $underleveraged = $Xn > ceil($X/$Lambda);
		if (!$underleveraged){ // along X
			if ($delta_Xn > 0){ // selling L-shares and X
				var $delta_Xn_inflection = $X * (( $Lambda/($Lambda-1) * ($a + ($b * $Lambda - 1) * $Yn/$Y) )**($b * $Lambda/($b*$Lambda-1)) - 1) / $Lambda;
				var $inflected = $delta_Xn > $delta_Xn_inflection;
			}
			const $dXn1 = $inflected ? $delta_Xn_inflection : $delta_Xn;
			$move_along_X($pool, $l_balances, $dXn1, $Leverage, $pool_props, $inverted);
			if ($inflected)
				$move_along_Y($pool, $l_balances, $delta_Xn - $delta_Xn_inflection, $Leverage, $pool_props, $inverted);
		}
		else{ // along Y
			if ($delta_Xn < 0){ // buying L-shares and X
				var $delta_Xn_inflection = -$b/($Lambda-1) * ($Lambda*$Xn - $X);
				var $inflected = abs($delta_Xn) > abs($delta_Xn_inflection);
			}
			const $dXn1 = $inflected ? $delta_Xn_inflection : $delta_Xn;
			$move_along_Y($pool, $l_balances, $dXn1, $Leverage, $pool_props, $inverted);
			if ($inflected)
				$move_along_X($pool, $l_balances, $delta_Xn - $delta_Xn_inflection, $Leverage, $pool_props, $inverted);
		}
	}

	const $final_l_balance = $l_balances[$L_key].balance;
	const $delta_l_balance = $final_l_balance - $initial_l_balance;
	const $final_P = $a/$b * ($pool.Y + $Y0) / ($pool.X + $X0);

	// the change in the total X balance of swap pool + all L-pools (positive when buying, negative when selling)
	const $net_delta = $delta_Xn + $pool.delta_XL + $delta_l_balance;

	const $final_shares = $initial_shares
		? floor($final_l_balance/$initial_l_balance / $pool.PL1_ratio * $initial_shares)
		: round($net_delta); // the initial units for shares are arbitrary
	const $shares = $final_shares - $initial_shares;
	$l_balances[$L_key].supply = $final_shares;
	const $avg_share_price = $net_delta/$shares;

	if ($final_shares == 0){
		require_cond($final_l_balance >= 0, "negative final l-balance after redeeming all shares: "+$final_l_balance);
		// any remaining l-balance will be a gift to those who buy the l-shares later
	//	$remainder_fee_X = $final_l_balance;
	//	$remainder_fee_Y = $final_l_balance * $final_P * ($Leverage-1)/$Leverage;
	//	$l_balances[$L_key].balance = 0;
	}

	const $denom = 1 - $pool.XL_denom/$b/($pool.X+$X0) - $pool.YL_denom/$a/($pool.Y+$Y0);
//	log('denom after L', $denom);
	require_cond($denom >= $singularity_threshold, "too close to the singularity point, denom="+$denom+", need more liquidity in order to buy this amount of L-tokens");

	$balances.x = $inverted ? $pool.Y : $pool.X;
	$balances.y = $inverted ? $pool.X : $pool.Y;
	$balances.xn = $inverted ? $pool.Yn : $pool.Xn;
	$balances.yn = $inverted ? $pool.Xn : $pool.Yn;

	// regular trading fee (%) and arb tax are paid on top
	let $min_P, $max_P, $recent_traded_amount = 0, $recent_paid_tax = 0;
	if ($recent.last_trade && $recent.last_trade.address == $trigger_initial_address && $recent.last_ts >= timestamp() - $trade_merge_period){
		$min_P = min($initial_P, $final_P, $direct ? $recent.last_trade.pmin : 1/$recent.last_trade.pmax);
		$max_P = max($initial_P, $final_P, $direct ? $recent.last_trade.pmax : 1/$recent.last_trade.pmin);
		$recent_traded_amount = $recent.last_trade.amounts[$token];
		$recent_paid_tax = $recent.last_trade.paid_taxes[$token];
	}
	else{
		$min_P = min($initial_P, $final_P);
		$max_P = max($initial_P, $final_P);
	}
	const $arb_profit_in_Y = ($max_P - $min_P) * ($recent_traded_amount + abs($net_delta)) / 2; // in Y
//	$arb_profit_in_Y = ($final_P - $initial_P) * $net_delta / 2; // in Y
//	require_cond($arb_profit_in_Y > 0, "arb profit "+$arb_profit_in_Y);
	const $arb_profit_in_X = $arb_profit_in_Y / $min_P;
	const $arb_profit_tax = $arb_profit_in_X * $pool_props.arb_profit_tax - $recent_paid_tax;
	const $swap_fee = abs($net_delta) * $pool_props.swap_fee;

	let $l_tax = 0;
	if ($delta_Xn > 0){ // sell
		const $gross_asset_out = -$net_delta; // gross means before tax and fees
		require_cond($gross_asset_out > 0, "asset out must be positive, got " + $gross_asset_out);
		if ($entry_price){
			// L>0 profit is accounted for in x tokens (in y tokens for L<0) meaning that only profits from the borrowed part of the L-pool are taxed
			$l_tax = max(($avg_share_price - $entry_price)*(-$shares)*$pool_props.leverage_profit_tax, 0);
		}
		else
			$l_tax = $gross_asset_out * $pool_props.leverage_token_tax;
	}
	
	// For buying, the fee is added on top. For selling (net_delta<0), the fees are subtracted
	const $subtotal_fee = $arb_profit_tax + $swap_fee + $l_tax;
	const $gross_delta_exact = $net_delta + $subtotal_fee;
	const $gross_delta = ceil($gross_delta_exact);
	const $rounding_fee = $gross_delta - $gross_delta_exact;
	const $total_fee = $subtotal_fee + $rounding_fee;

//	log({$gross_delta, $gross_delta_exact, $net_delta, $trading_fee, $l_tax, $arb_profit_tax, $arb_profit_in_X, $arb_profit_in_Y, $min_P, $max_P, $delta_l_balance, $delta_Xn, $pool})

//	log('balances before', $balances);
	$add_net_balance_without_changing_price($balances, $profits, $token, $total_fee, $Lambda);
//	log('balances after', $balances);

	$update_recent_data($recent, $inverted ? 1/$initial_P : $initial_P, $inverted ? 1/$final_P : $final_P, $trigger_initial_address, $token, abs($net_delta), $arb_profit_tax, $pool_props.period_length);

	const $event = JSON.stringify({
		type: 'leverage',
		token: $token,
		L: $Leverage,
		shares: $shares,
		amount: $gross_delta,
		swap_fee: $swap_fee,
		arb_profit_tax: $arb_profit_tax,
		l_tax: $l_tax,
		total_fee: $total_fee,
	});

	return {
		shares: $shares,
		net_delta: $net_delta,
		gross_delta: $gross_delta,
		avg_share_price: $avg_share_price,
		arb_profit_tax: $arb_profit_tax,
		l_tax: $l_tax,
		swap_fee: $swap_fee,
		total_fee: $total_fee,
		initial_price: $initial_P,
		final_price: $final_P,
		event: $event,
	}
};


const $handle_trade_l_shares_request = ($pool_vars, $balances, $l_balances, $profits, $recent, $x0, $y0, $trigger_data, $trigger_address, $trigger_outputs, $trigger_initial_address, $pool_props) => {
	const $x_asset = $pool_props.x_asset;
	const $y_asset = $pool_props.y_asset;

	const $asset = $trigger_data.asset == 'x' ? $x_asset : ($trigger_data.asset == 'y' ? $y_asset : $trigger_data.asset);
	const $L = $trigger_data.L;
	const $buy = $trigger_data.buy;
	const $sell = $trigger_data.sell;
	const $delta = $trigger_data.delta;
	const $received_amount = $trigger_outputs[$asset];
	const $min_received_amount = $asset == 'base' ? 10000 : 0;
	const $net_received_amount = $received_amount - $min_received_amount;
	
	require_cond(!($buy && $sell), "buy or sell?");
	require_cond($delta > 0, "delta must be positive");
	if ($buy)
		require_cond($net_received_amount > 0, "you forgot to pay");
	else
		require_cond($net_received_amount == 0, "don't send asset");
	const $delta_Xn = $buy ? -$delta : $delta; // Xn in the pool
	const $asset_label = $asset == $x_asset ? 'x' : 'y';

	const $signedL = $asset_label == 'x' ? $L : -$L;
	if ($buy && $trigger_data.tokens || $sell && !$trigger_data.position){
		var $l_shares_asset = $pool_vars['leveraged_asset' + $signedL];
		require_cond($l_shares_asset, "please define an asset for the leveraged token first");
	}
	if ($sell){
		if ($trigger_data.position){
			var $position = $pool_vars[$trigger_data.position];
			require_cond($position, "no such position");
			require_cond($position.owner == $trigger_address, "you are not the owner of this position");
			const $parts = split($trigger_data.position, '_');
			require_cond(+$parts[1] == $signedL, "wrong L");
			var $shares_in = $position.shares;
		}
		else{
			var $shares_in = $trigger_outputs[$l_shares_asset];
			require_cond($shares_in > 0, "no leveraged tokens received");
		}
	}

	let $res = $trade_l_shares($balances, $l_balances, $profits, $recent, $x0, $y0, $L, $asset, $delta_Xn, $position.price, $trigger_initial_address, $pool_props);
//	log('balances', $balances, 'res', $res);

	const $shares = $res.shares;
	const $gross_delta = $res.gross_delta;
	
	if ($buy){
		var $asset_out = $received_amount - $gross_delta; // the change
		require_cond($asset_out >= 0, "expected " + $gross_delta + ", received " + $received_amount);
	}
	else{
		var $shares_change = $shares_in + $shares; // shares < 0
		require_cond($shares_change >= 0, "expected " + (-$shares) + " shares, received " + $shares_in);
		var $asset_out = -$gross_delta;
	}

	$res.signedL = $signedL;
	$res.asset_label = $asset_label;
	$res.asset = $asset;
	$res.l_shares_asset = $l_shares_asset;
	$res.position = $position;
	$res.shares_change = $shares_change;
	$res.asset_out = $asset_out;
	return $res
};



const $validate_and_apply_new_governed_param_value = ($name, $value, $balances, $l_balances, $profits, $lp_shares, $pool_props, $locked_governance) => {

	if ($locked_governance)
		require_cond(!$locked_governance[$name], "governance is not allowed to change "+$name);
	
	const $Lambda = $pool_props.Lambda;
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	const $gamma = $pool_props.gamma;
	const $mid_price = $pool_props.mid_price;
	const $mid_price_beta = $pool_props.mid_price_beta;

	const $s_curve = $lp_shares.linear * $lp_shares.coef;
	const $x0 = $mid_price ? $s_curve / $mid_price_beta / $gamma : 0;
	const $y0 = $x0 * $mid_price;

	if ($name == 'pool_leverage'){
		require_cond($profits.x < 1 && $profits.y < 1, "profits must be added to the pool before changing pool_leverage"); // only rounding fees are allowed
		$profits.x = 0;
		$profits.y = 0;
		require_cond($alpha != 1/$value, "pool leverage = 1/alpha");
		require_cond($beta != 1/$value, "pool leverage = 1/beta");
		require_cond($value != $Lambda, "same Lambda");
		if ($value > 1)
			require_cond(!$mid_price, "price range setting is incompatible with new pool leverage");
		$balances.x = $balances.x * $value/$Lambda;
		$balances.y = $balances.y * $value/$Lambda;
		if ($value == 1){
			// move the excessive balances to profits
			$profits.x = $profits.x + $balances.xn - $balances.x;
			$profits.y = $profits.y + $balances.yn - $balances.y;
			$balances.xn = $balances.x;
			$balances.yn = $balances.y;
		}
		// we have modified balances and profits
	}
	else if ($name == 'mid_price' || $name == 'price_deviation'){
		require_cond($value && $mid_price, $name+" must be nonzero");
		require_cond($alpha == 0.5, "equal weights only");
		let $new_p0, $new_gamma;
		if ($name == 'price_deviation'){
			require_cond($value > 1, "price deviation must be > 1");
			$new_p0 = $mid_price;
			$new_gamma = $value;
		}
		else{
			$new_p0 = $value;
			$new_gamma = $gamma;
		}
		const $sqp = $mid_price_beta; // sqrt(mid_price)
		const $new_sqp = sqrt($new_p0);
		const $x = $balances.x;
		const $y = $balances.y;

		// 1. assume we keep all x and decrease y
		let $new_s;
		const $new_s1 = 1 / (1/$s_curve + (1/$gamma/$sqp - 1/$new_gamma/$new_sqp) / $x);
		const $new_y = $new_s1 * ($y/$s_curve + $sqp/$gamma - $new_sqp/$new_gamma);
		if ($new_y <= $y){
			require_cond($new_y > 0, "new y is negative");
			require_cond($new_s1 > 0, "new s1 is negative");
			$profits.y = $profits.y + $y - $new_y;
			$balances.y = $new_y;
			$balances.yn = $new_y;
			$lp_shares.coef = $lp_shares.coef * $new_s1/$s_curve;
			$new_s = $new_s1;
		}
		else{ // 2. keep all y and decrease x
			const $new_s2 = 1 / (1/$s_curve + ($sqp/$gamma - $new_sqp/$new_gamma) / $y);
			const $new_x = $new_s2 * ($x/$s_curve + 1/$gamma/$sqp - 1/$new_gamma/$new_sqp);
			require_cond($new_x <= $x, "can't adjust x and y to keep the price");
			require_cond($new_x > 0, "new x is negative");
			require_cond($new_s2 > 0, "new s2 is negative");
			$profits.x = $profits.x + $x - $new_x;
			$balances.x = $new_x;
			$balances.xn = $new_x;
			$lp_shares.coef = $lp_shares.coef * $new_s2/$s_curve;
			$new_s = $new_s2;
		}
		var $new_x0 = $new_s / $new_sqp / $new_gamma;
		var $new_y0 = $new_s * $new_sqp / $new_gamma;

		/*
		$sqp = sqrt($new_p0);
		$b = ($balances.x/$s_curve*$sqp + $balances.y/$s_curve/$sqp)/$new_gamma;
		$a = $balances.x*$balances.y/$s_curve/$s_curve;
		$lp_shares.coef = $lp_shares.coef / (-$b + sqrt($b*$b - 4*$a*(1/$new_gamma/$new_gamma-1)))*2*$a;
		*/

		// we have modified lp_shares, balances, and profits
	}
	else if ($name == 'alpha'){
		require_cond(!$mid_price, "can't change token weights while trading in limited range");
		const $new_alpha = $value;
		const $new_beta = 1 - $new_alpha;
		require_cond($new_alpha != 1/$Lambda && $new_beta != 1/$Lambda, "pool leverage = 1/alpha or 1/beta");
		// s_coef is unused
	//	var['s_coef'] *= $balances.xn**$value * $balances.yn**(1-$value) / $s_curve;

		// change the balances to preserve the price p = alpha/beta * y/x = const
		const $new_y2x = $new_beta/$new_alpha * $alpha/$beta * $balances.y/$balances.x;
		if ($Lambda > 1){
			if ($balances.xn * $Lambda * $new_y2x <= $balances.yn * $Lambda){ // x fully leveraged
				$balances.x = $balances.xn * $Lambda;
				$balances.y = $balances.x * $new_y2x;
			}
			else if ($balances.yn * $Lambda / $new_y2x <= $balances.xn * $Lambda){ // y fully leveraged
				$balances.y = $balances.yn * $Lambda;
				$balances.x = $balances.y / $new_y2x;
			}
			else
				bounce("can't preserve the price"); // should never happen
		}
		else {
			const $new_y = $balances.xn * $new_y2x; // assuming x balance stays unchanged
			if ($new_y <= $balances.yn){ // excessive y
				$profits.y = $profits.y + $balances.yn - $new_y;
				$balances.yn = $new_y;
				$balances.y = $new_y;
			}
			else { // excessive x
				const $new_x = $balances.yn / $new_y2x; // assuming y balance stays unchanged
				require_cond($new_x <= $balances.xn, "neither excessive x nor excessive y"); // should never happen
				$profits.x = $profits.x + $balances.xn - $new_x;
				$balances.xn = $new_x;
				$balances.x = $new_x;
			}
		}
		// balances and profits modified
	}

	// under new balances, we might go over (or too close to) the singularity point
	const $denom = 1 - $get_utilization_ratio($balances, $l_balances, $new_x0 || $x0, $new_y0 || $y0, $new_alpha || $alpha);
	require_cond($denom >= $singularity_threshold, "new balances would bring us too close to the singularity point, denom="+$denom);
};



// end of AA functions






function getPoolState(aaParams, stateVars) {
	const { balances, leveraged_balances, profits, recent, lp_shares } = stateVars;
	const { x_asset, y_asset } = aaParams;
	const getParam = (name, default_value) => {
		if (stateVars[name] !== undefined)
			return stateVars[name];
		if (aaParams[name] !== undefined)
			return aaParams[name];
		return default_value;
	};
	const alpha = getParam('alpha', 0.5);
	const beta = 1 - alpha;
	const mid_price = getParam('mid_price', 0);
	const gamma = getParam('price_deviation', 0);
	let x0 = 0, y0 = 0, p_max, p_min;
	if (mid_price) {
		const s = lp_shares.linear;
		const s_curve = s * lp_shares.coef;
		x0 = s_curve / mid_price ** beta / gamma;
		y0 = x0 * mid_price;
		p_max = alpha / beta * gamma ** (1 / beta) * mid_price;
		p_min = alpha / beta / gamma ** (1 / alpha) * mid_price;
	}
	const shifts = { x0, y0 };
	const bounds = { p_max, p_min };
	const pool_props = {
		alpha,
		beta,
		gamma,
		mid_price,
		Lambda: getParam('pool_leverage', 1),
		swap_fee: getParam('swap_fee', 0.003),
		exit_fee: getParam('exit_fee', 0.005),
		arb_profit_tax: getParam('arb_profit_tax', 0),
		leverage_profit_tax: getParam('leverage_profit_tax', 0),
		leverage_token_tax: getParam('leverage_token_tax', 0),
		period_length: getParam('period_length', 3600),
		base_interest_rate: getParam('base_interest_rate', 0.2),
		x_asset,
		y_asset,
		shares_bonding_curve: aaParams.shares_bonding_curve || 'IXBHF6T4IKMYAFGRM54F5FVMXGKCTFNT',
	};
	return { balances, leveraged_balances, profits, recent, lp_shares, pool_props, shifts, bounds };
}

function toAsset(token, pool_props) {
	const { x_asset, y_asset } = pool_props;
	if (token === 'x')
		return x_asset;
	if (token === 'y')
		return y_asset;
	require_cond(token === x_asset || token === y_asset, `unknown token ${token}`);
	return token;
}

function toXY(token, pool_props) {
	const { x_asset, y_asset } = pool_props;
	if (token === x_asset)
		return 'x';
	if (token === y_asset)
		return 'y';
	require_cond(token === 'x' || token === 'y', `unknown token ${token}`);
	return token;
}

function getCurrentUtilizationRatio(poolState) {
	const { balances, leveraged_balances, shifts: { x0, y0 }, pool_props: { alpha } } = poolState;
	return $get_utilization_ratio(balances, leveraged_balances, x0, y0, alpha);
}

function getInterestRate(poolState) {
	return poolState.pool_props.base_interest_rate / (1 - getCurrentUtilizationRatio(poolState));
}

// modifies balances, leveraged_balances, and profits fields of poolState
function chargeInterest(poolState) {
	const { balances, leveraged_balances, profits, recent: { last_ts }, shifts: { x0, y0 }, pool_props } = poolState;
	const { alpha, Lambda } = pool_props;
	const i = getInterestRate(poolState);
	$charge_interest(balances, leveraged_balances, profits, x0, y0, last_ts, i, alpha, Lambda);
}

function getSwapParams(in_amount, in_token, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, profits, recent, shifts: { x0, y0 }, pool_props } = poolState;

	in_token = toXY(in_token, pool_props);
	const y_in = in_token === 'y';
	
	const { res, required_amount, param_value } = findParamToMatchAmount(in_amount, in_amount, delta_Yn => {
		const res = $swap(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), _.cloneDeep(profits), _.cloneDeep(recent), x0, y0, y_in, delta_Yn, 0, -1, 0, 'ADDRESS', pool_props);
		return { res, required_amount: res.amount_Y };
	});
	return { res, delta_Yn: param_value };
}

function getSwapParamsByOutput(out_amount, out_token, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, profits, recent, shifts: { x0, y0 }, pool_props } = poolState;

	out_token = toXY(out_token, pool_props);
	const y_in = out_token === 'x';

	const in_token = out_token === 'x' ? 'y' : 'x';
	const delta_Yn_initial_estimation = round(balances[in_token + 'n'] * out_amount / (balances[out_token + 'n'] - out_amount));
	
	const { res, required_amount, param_value } = findParamToMatchAmount(out_amount, delta_Yn_initial_estimation, delta_Yn => {
		const res = $swap(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), _.cloneDeep(profits), _.cloneDeep(recent), x0, y0, y_in, delta_Yn, 0, -1, 0, 'ADDRESS', pool_props);
		return { res, required_amount: res.net_amount_X };
	});
	return { res, delta_Yn: param_value };
}

function getLeveragedBuyParams(in_amount, in_token, leverage, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, profits, recent, shifts: { x0, y0 }, pool_props } = poolState;

	in_token = toAsset(in_token, pool_props);

	const { res, required_amount, param_value } = findParamToMatchAmount(in_amount, in_amount, delta => {
		const res = $trade_l_shares(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), _.cloneDeep(profits), _.cloneDeep(recent), x0, y0, leverage, in_token, -delta, 0, 'ADDRESS', pool_props);
		return { res, required_amount: res.gross_delta };
	});
	return { res, delta: param_value };
}

function getLeveragedSellParams(in_amount, token, leverage, entry_price, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, profits, recent, shifts: { x0, y0 }, pool_props } = poolState;

	token = toAsset(token, pool_props);

	const { res, required_amount, param_value } = findParamToMatchAmount(in_amount, in_amount * (leverage - 1), delta => {
		const res = $trade_l_shares(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), _.cloneDeep(profits), _.cloneDeep(recent), x0, y0, leverage, token, delta, entry_price, 'ADDRESS', pool_props);
		return { res, required_amount: -res.shares };
	});
	return { res, delta: param_value };
}

function getLinearSharesFunction(shares_bonding_curve) {
	switch (shares_bonding_curve) {
		case 'IXBHF6T4IKMYAFGRM54F5FVMXGKCTFNT':
			return ($issued_shares) => $issued_shares;
		case 'FVFJQZVUWUANWRWXJ5LWVYDUP2XF7BIB':
			return ($issued_shares) => $issued_shares ** 2;
	}
	throw Error(`unknown bonding curve ${shares_bonding_curve}`);
}

function getRedemptionResult(received_shares_amount, asset, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, profits, recent, shifts: { x0, y0 }, lp_shares, pool_props } = poolState;

	if (asset)
		asset = toAsset(asset, pool_props);

	const get_linear_shares = getLinearSharesFunction(pool_props.shares_bonding_curve);
	const new_issued = lp_shares.issued - received_shares_amount;
	const new_linear = get_linear_shares(new_issued);

	const res = $redeem_shares(lp_shares.linear, balances, leveraged_balances, profits, recent, x0, y0, lp_shares.linear - new_linear, asset, pool_props);
	
	return res;
}



function findParamToMatchAmount(target_amount, initial_estimation, f) {
	
	let param_value = initial_estimation; // initial estimation

	let bestAbove = {
		distance: Infinity,
		required_amount: null,
		param_value: null,
	};

	let bestBelow = {
		distance: -Infinity,
		required_amount: null,
		param_value: null,
	};

	let prev_param_value;
	let prev_distance = Infinity;
	let prev_slope;
	let prev_required_amount;
	let count = 0;
	while (true) {
		count++;
		if (count > 100)
			throw Error(`too many iterations, target ${target_amount}, last ${required_amount}`)
		log(`${count}: trying value ${param_value}`);
		try {
			var { required_amount, res } = f(param_value);
		}
		catch (e) {
			log(`value ${param_value} failed`, e);
			param_value = (param_value + (prev_param_value || 0)) / 2;
			continue;
		}
		const distance = target_amount - required_amount;
		if (
			bestAbove.distance < Infinity && bestBelow.distance > -Infinity
			&& (distance > 0 && distance >= bestAbove.distance || distance < 0 && distance <= bestBelow.distance)
		) {
			log(`distance ${distance} out of best range, will try its middle`);
			param_value = (bestAbove.param_value + bestBelow.param_value) / 2;
			continue;
		}
		if (distance > 0 && distance < bestAbove.distance)
			bestAbove = { distance, required_amount, param_value };
		if (distance < 0 && distance > bestBelow.distance)
			bestBelow = { distance, required_amount, param_value };
		const approach = prev_distance / distance;
		const delta_param_value = param_value - prev_param_value;

		// 1st derivative
		let slope = prev_param_value ? (param_value - prev_param_value) / (required_amount - prev_required_amount) : param_value / required_amount;
	//	if (distance < 1000) // too noisy probably due to rounding
	//		slope = param_value / required_amount;
		
		log(`result`, { param_value, required_amount, res, distance, approach, delta_param_value, slope });
		if (required_amount === target_amount)
			return { res, required_amount, param_value };
		if (param_value === prev_param_value) {
			log(`would repeat value ${param_value}`);
			return { res, required_amount, param_value };
		}
		if (required_amount === prev_required_amount) {
			log(`repeated amount ${required_amount}`);
			return { res, required_amount, param_value };
		}

		prev_param_value = param_value;
		param_value += slope * (target_amount - required_amount);

		if (0 && prev_slope) { // 2nd term of Taylor series
			const second_derivative = (slope - prev_slope) / (required_amount - prev_required_amount);
			param_value += 1 / 2 * second_derivative * (target_amount - required_amount) ** 2;
			log('2nd derivative', 1 / 2 * second_derivative * (target_amount - required_amount) ** 2)
		}
	//	param_value = round(param_value);

		if (param_value < 0) {
			log(`next param value would be negative ${param_value}, will half instead`)
			param_value = prev_param_value / 2;
		}

		prev_distance = distance;
		prev_slope = prev_required_amount && slope;
		prev_required_amount = required_amount;
	}
}

exports.setLogger = setLogger;

exports.$swap = $swap;
exports.$trade_l_shares = $trade_l_shares;

exports.getPoolState = getPoolState;
exports.toXY = toXY;
exports.toAsset = toAsset;
exports.getCurrentUtilizationRatio = getCurrentUtilizationRatio;
exports.getInterestRate = getInterestRate;
exports.chargeInterest = chargeInterest;
exports.getSwapParams = getSwapParams;
exports.getSwapParamsByOutput = getSwapParamsByOutput;
exports.getLeveragedBuyParams = getLeveragedBuyParams;
exports.getLeveragedSellParams = getLeveragedSellParams;
exports.getRedemptionResult = getRedemptionResult;

