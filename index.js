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

const $get_leverages = () => [2, 5, 10, 20, 50, 100];

const $singularity_threshold = 0.1;

const $get_utilization_ratio = ($balances, $l_balances, $x0, $y0, $alpha) => {
	const $beta = 1 - $alpha;
	const $ratio = $get_leverages().reduce(($acc, $L) =>
		$acc +
		($l_balances[$L + 'x']?.balance || 0) / ($balances.x + $x0) * ($L - 1) / $beta +
		($l_balances[-$L + 'x']?.balance || 0) / ($balances.y + $y0) * ($L - 1) / $alpha,
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
	return round($final_X)
};

// along x means keeping x fully leveraged (y underleveraged)
const $get_final_x_along_x = ($X, $Y, $final_Y, $pool_props, $inverted) => {
	const $b = $inverted ? $pool_props.alpha : $pool_props.beta; // beta
	return round($X * ($final_Y/$Y)**($b * $pool_props.Lambda/($b * $pool_props.Lambda - 1)))
};

// along y means keeping y fully leveraged (x underleveraged)
const $get_final_x_along_y = ($X, $Y, $final_Y, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	return round($X * ($final_Y/$Y)**(1-1/$a/$pool_props.Lambda))
};


// Y through X:

// without LP leverage (Lambda)
const $get_final_y = ($X, $Y, $final_X, $X0, $Y0, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_Y = ($Y + $Y0) * (($X + $X0)/($final_X + $X0))**($a/$b) - $Y0;
	require_cond($final_Y >= 0, "bad final_Y " + $final_Y);
	const $deltaY = $final_Y - $Y;
	return round($final_Y)
};

// along x means keeping x fully leveraged (y underleveraged)
const $get_final_y_along_x = ($X, $Y, $final_X, $pool_props, $inverted) => {
	const $b = $inverted ? $pool_props.alpha : $pool_props.beta; // beta
	return round($Y * ($final_X/$X)**(1 - 1/$b/$pool_props.Lambda))
};

// along y means keeping y fully leveraged (x underleveraged)
const $get_final_y_along_y = ($X, $Y, $final_X, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	return round($Y * ($final_X/$X)**($a*$pool_props.Lambda/($a*$pool_props.Lambda - 1)))
};


// X, Y through P:

// without LP leverage (Lambda)
const $get_final_xy = ($X, $Y, $P, $final_P, $X0, $Y0, $pool_props, $inverted) => {
	require_cond($final_P >= $P, "not selling Y");
	log('get_final_xy', $X, $Y, $P, $final_P, $X0, $Y0);
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_X = ($X + $X0) * ($P/$final_P)**$b - $X0;
	const $final_Y = $b/$a * $final_P * ($final_X + $X0) - $Y0;
	const $deltaX = $X - $final_X;
//	log({$final_X, $final_Y, $a, $b})
	require_cond($final_X >= 0, "bad final_X " + $final_X);
	require_cond($final_Y >= 0, "bad final_Y " + $final_Y);
	require_cond($deltaX >= 0, "bad deltaX " + $deltaX);
	return {
		X: round($final_X),
		Y: round($final_Y),
	}
};

// along x means keeping x fully leveraged (y underleveraged)
const $get_final_xy_along_x = ($X, $Y, $P, $final_P, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_X = $X * ($P/$final_P)**($b*$pool_props.Lambda);
	const $final_Y = $b/$a * $final_P * $final_X;
	return {
		X: round($final_X),
		Y: round($final_Y),
	}
};

// along y means keeping y fully leveraged (x underleveraged)
const $get_final_xy_along_y = ($X, $Y, $P, $final_P, $pool_props, $inverted) => {
	const $a = $inverted ? $pool_props.beta : $pool_props.alpha; // alpha
	const $b = 1 - $a; // beta
	const $final_Y = $Y * ($final_P/$P)**($a*$pool_props.Lambda);
	const $final_X = $a/$b * $final_Y / $final_P;
	return {
		X: round($final_X),
		Y: round($final_Y),
	}
};



const $add_net_balance_without_changing_price = ($balances, $side, $amount, $Lambda) => {
	require_cond($Lambda > 1, "Lambda must be > 1");

	const $opposite = $side === 'x' ? 'y' : 'x';
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
	if (!$underleveraged){
		// Y is underleveraged, increase Y proportionally while keeping Yn intact
		const $full_delta_Y = $Y * $delta_Xn/$Xn;
		if ($Y + $full_delta_Y > $Yn * $Lambda){ // would overshoot and make Y overleveraged
			const $ratio = $Yn * $Lambda / $Y - 1;
			$delta_X = round($ratio * $X);
			$delta_Y = round($ratio * $Y);
		}
		else{
			$delta_X = round($delta_Xn * $Lambda);
			$delta_Y = round($full_delta_Y);
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



const $charge_interest = ($balances, $l_balances, $profits, $x0, $y0, $last_ts, $i, $alpha, $Lambda) => {
	require_cond($last_ts, "no last ts");
	const $beta = 1 - $alpha;
	const $x = $balances.x;
	const $y = $balances.y;
	const $accrued_rate = (round(Date.now()/1000) - $last_ts)/3600/24/360 * $i;
	const $y2x = ($y + $y0) / ($x + $x0);
	const $p = $alpha/$beta * $y2x;
	let $n_deltas = {dxn:0, dyn:0};
	$get_leverages().forEach(($L) => {
		const $xL = $l_balances[$L+'x']?.balance;
		const $yL = $l_balances[-$L+'x']?.balance;
		if ($xL){
			const $dxL = -min(round($xL * ($L-1) * $accrued_rate), $xL);
			$l_balances[$L+'x'].balance = $xL + $dxL;
			// change in the amount lent out by the swap pool (swap pool's assets)
			const $delta_yn = $dxL * $p * ($L-1)/$L; // < 0
			$n_deltas.dyn = $n_deltas.dyn + $delta_yn;
			if ($Lambda === 1){
				$n_deltas.dxn = $n_deltas.dxn + $delta_yn / $y2x; // proportional - no price change
				$profits.x = $profits.x - $dxL - $delta_yn / $y2x;
			}
			else
				$n_deltas.dxn = -$dxL; // > 0
		}
		if ($yL){
			const $dyL = -min(round($yL * ($L-1) * $accrued_rate), $yL);
			$l_balances[-$L+'x'].balance = $yL + $dyL;
			// change in the amount lent out by the swap pool (swap pool's assets)
			const $delta_xn = $dyL / $p * ($L-1)/$L; // < 0
			$n_deltas.dxn = $n_deltas.dxn + $delta_xn; 
			if ($Lambda === 1){
				$n_deltas.dyn = $n_deltas.dyn + $delta_xn * $y2x; // proportional - no price change
				$profits.y = $profits.y - $dyL - $delta_xn * $y2x;
			}
			else
				$n_deltas.dyn = -$dyL; // > 0
		}
	});
	const $dxn = round($n_deltas.dxn);
	const $dyn = round($n_deltas.dyn);
	if ($Lambda === 1){
		$profits.x = round($profits.x);
		$profits.y = round($profits.y);
		$balances.xn = $balances.xn + $dxn;
		$balances.yn = $balances.yn + $dyn;
		$balances.x = $balances.x + $dxn;
		$balances.y = $balances.y + $dyn;
	}
	else{
		$add_net_balance_without_changing_price($balances, 'x', $dxn, $Lambda);
		$add_net_balance_without_changing_price($balances, 'y', $dyn, $Lambda);
	}
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

const $update_leveraged_balances = ($x, $y, $final_x, $final_y, $x0, $y0, $l_balances, $alpha, $inverted) => {
	const $beta = 1 - $alpha;

	const $p = $alpha/$beta * ($y + $y0) / ($x + $x0); // price of x in terms of y
	const $P = $inverted ? 1/$p : $p; // price of X in terms of Y
	const $final_p = $alpha/$beta * ($final_y + $y0) / ($final_x + $x0);
	const $final_P = $inverted ? 1/$final_p : $final_p;

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
		const $balance = $l_balances[$allyL+'x']?.balance;
		const $obalance = $l_balances[-$allyL+'x']?.balance;
		if (!$balance && !$obalance)
			return;
		const $ratio_L1 = $pow($ratio_powers, $L) / $ratio;
		const $debt_ratio = ($L-1)/$L;
		if ($balance) {
			const $delta_XL_balance = round($balance * ($ratio_L1 - 1));
			const $new_XL_balance = $balance + $delta_XL_balance;
			$l_balances[$allyL+'x'].balance = $new_XL_balance;
			const $delta_YL_balance = -(($new_XL_balance * $final_P - $balance * $P) * $debt_ratio); // borrowed
			$totals.delta_XL = $totals.delta_XL + $delta_XL_balance;
			$totals.delta_YL = $totals.delta_YL + $delta_YL_balance;
			$totals.XL_denom = $totals.XL_denom + $new_XL_balance * ($L-1);
		}
		if ($obalance) { // e.g. L=-2
			const $delta_YL_obalance = round($obalance * (1/$ratio_L1 - 1));
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



const $swap_by_delta_y = ($balances, $l_balances, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $pool_props) => {
	
	require_cond(!$in_final_P, "no final price please, this is swap by Y");
	
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	const $Lambda = $pool_props.Lambda;

	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;

	let $inverted, $X, $Y, $Xn, $Yn, $X0, $Y0, $a, $b;
	
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
	}
	require_cond($delta_Yn > 0 && is_integer($delta_Yn), "bad delta " + $delta_Yn);

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
		const $delta_Y = -round(($b*$Lambda-1)/$a*$delta_Yn);
		$final_Y = $Y + $delta_Y;
		$final_X = $get_final_x_along_x($X, $Y, $final_Y, $pool_props, $inverted);
		$final_Xn = round($final_X/$Lambda);
	}
	else if ($underleveraged){
		const $delta_Yn_inflection = round($Y * (( $Lambda/($Lambda-1) * ($b + ($a * $Lambda - 1) * $Xn/$X) )**($a * $Lambda/($a*$Lambda-1)) - 1) / $Lambda);
		const $inflected = $delta_Yn > $delta_Yn_inflection;
		// along Y until the inflection point
		const $inflection_Yn = $Yn + $delta_Yn_inflection;
		const $final_Yn1 = $inflected ? $inflection_Yn : $final_Yn;
		const $final_Y1 = round($final_Yn1 * $Lambda);
		const $final_X1 = $get_final_x_along_y($X, $Y, $final_Y1, $pool_props, $inverted);
		const $delta_X1 = $final_X1 - $X;
		const $delta_Xn1 = -round($b/($a*$Lambda-1) * $delta_X1);
		const $final_Xn1 = $Xn + $delta_Xn1;
		if ($inflected){
			// then, along X
			const $delta_Yn2 = $final_Yn - $final_Yn1;
			const $delta_Y2 = -round(($b*$Lambda-1)/$a*$delta_Yn2);
			$final_Y = $final_Y1 + $delta_Y2;
			$final_X = $get_final_x_along_x($final_X1, $final_Y1, $final_Y, $pool_props, $inverted);
			$final_Xn = round($final_X/$Lambda);
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

	// if inverted, XL corresponds to y, YL to x
	const $totals = $update_leveraged_balances($x, $y, $final_x, $final_y, $x0, $y0, $l_balances, $alpha, $inverted);

	const $amount_X = floor(-($final_Xn - $X + $totals.delta_XL));
	const $amount_Y = ceil($final_Yn - $Yn + $totals.delta_YL);
	if ($received_amount_Y >= 0)
		require_cond($received_amount_Y >= $amount_Y, "expected " + $amount_Y + ", received " + $received_amount_Y);
	require_cond($amount_X >= 0, "to pay " + $amount_X);
	if ($min_amount_out)
		require_cond($amount_X >= $min_amount_out, "output amount " + $amount_X + " would be less than the expected minimum " + $min_amount_out);
	const $change = $received_amount_Y - $amount_Y;

	const $denom = 1 - $totals.XL_denom/$b/($final_X+$X0) - $totals.YL_denom/$a/($final_Y+$Y0);
	log('denom after L', $denom);
	require_cond($denom >= $singularity_threshold, "too close to the singularity point, denom="+$denom+", need more liquidity in order to swap this amount");

	// arb tax based on price difference
	const $p = $alpha/$beta * ($y + $y0) / ($x + $x0); // price of x in terms of y
	const $P = $inverted ? 1/$p : $p; // price of X in terms of Y
	const $final_p = $alpha/$beta * ($final_y + $y0) / ($final_x + $x0);
	const $final_P = $inverted ? 1/$final_p : $final_p;
	require_cond($final_P > $P, "price should have risen but hasn't, old " + $P + ", new " + $final_P);

	const $arb_profit_in_Y = ($final_P - $P) * $amount_X / 2; // in Y
	const $arb_profit_in_X = $arb_profit_in_Y / $final_P;
	const $fee = ceil($arb_profit_in_X * $pool_props.arb_profit_tax + $amount_X * $pool_props.swap_fee);
	const $net_amount_X = $amount_X - $fee;

	// add the fee to the pool without trading and affecting the price (Lambda>1) or to a separate profit accumulator (Lambda=1)
	// add the fee to the pool, this might affect the price amd make L-pools trade
	if ($Lambda === 1){
	}
	else{
		$add_net_balance_without_changing_price($balances, $y_in ? 'x' : 'y', $fee, $Lambda);
	}

	return {
		net_amount_X: $net_amount_X,
		amount_Y: $amount_Y,
		fee: $fee,
		change: $change,
	}
};



const $swap_by_final_p = ($balances, $l_balances, $x0, $y0, $y_in, $in_delta_Yn, $final_P, $received_amount_Y, $min_amount_out, $pool_props) => {
			
	require_cond(!$in_delta_Yn, "no delta Yn please, this is swap by P");
	
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	const $Lambda = $pool_props.Lambda;

	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;
	
	let $inverted, $X, $Y, $Xn, $Yn, $X0, $Y0, $a, $b;

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
		$final_Xn = round($final_X/$Lambda);
		$delta_Y = $final_Y - $Y;
		$delta_Yn = -round($a/($b*$Lambda-1)*$delta_Y);
		$final_Yn = $Yn + $delta_Yn;
	}
	else if ($underleveraged){
		const $inflection_P = $P * ( $Lambda/($Lambda-1) * ($b + ($a * $Lambda - 1) * $Xn/$X) )**(1/($a*$Lambda-1));
		const $inflected = $final_P > $inflection_P;
		// along Y until the inflection point
		const $final_P1 = $inflected ? $inflection_P : $final_P;
		const $final1 = $get_final_xy_along_y($X, $Y, $P, $final_P1, $pool_props, $inverted);
		const $final_X1 = $final1.X;
		const $final_Y1 = $final1.Y;
		const $final_Yn1 = round($final_Y1 / $Lambda);
		const $delta_X1 = $final_X1 - $X;
		const $delta_Xn1 = -round($b/($a*$Lambda-1) * $delta_X1);
		const $final_Xn1 = $Xn + $delta_Xn1;
		if ($inflected){
			// then, along X
			const $final = $get_final_xy_along_x($final_X1, $final_Y1, $final_P1, $final_P, $pool_props, $inverted);
			$final_X = $final.X;
			$final_Y = $final.Y;
			$final_Xn = round($final_X/$Lambda);
			const $delta_Y2 = $final_Y - $final_Y1;
			const $delta_Yn2 = -round($a/($b*$Lambda-1)*$delta_Y2);
			$final_Yn = $final_Yn1 + $delta_Yn2;
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
	log("balances after swap", $balances);

	const $final_y = $balances.y;
	const $final_x = $balances.x;

	// if inverted, XL corresponds to y, YL to x
	const $totals = $update_leveraged_balances($x, $y, $final_x, $final_y, $x0, $y0, $l_balances, $alpha, $inverted);

	const $amount_X = floor(-($final_Xn - $X + $totals.delta_XL));
	const $amount_Y = ceil($final_Yn - $Yn + $totals.delta_YL);
	if ($received_amount_Y >= 0)
		require_cond($received_amount_Y >= $amount_Y, "expected " + $amount_Y + ", received " + $received_amount_Y);
	require_cond($amount_X >= 0, "to pay " + $amount_X);
	if ($min_amount_out)
		require_cond($amount_X >= $min_amount_out, "output amount " + $amount_X + " would be less than the expected minimum " + $min_amount_out);
	const $change = $received_amount_Y - $amount_Y;

	const $denom = 1 - $totals.XL_denom/$b/($final_X+$X0) - $totals.YL_denom/$a/($final_Y+$Y0);
	log('denom after swap to price:', $denom);
	require_cond($denom >= $singularity_threshold, "too close to the singularity point, denom="+$denom+", need more liquidity in order to swap this amount");

	// arb tax based on price difference
	require_cond($final_P > $P, "price should have risen but hasn't, old " + $P + ", new " + $final_P);

	const $arb_profit_in_Y = ($final_P - $P) * $amount_X / 2; // in Y
	const $arb_profit_in_X = $arb_profit_in_Y / $final_P;
	const $fee = ceil($arb_profit_in_X * $pool_props.arb_profit_tax + $amount_X * $pool_props.swap_fee);
	const $net_amount_X = $amount_X - $fee;

	// add the fee to the pool without trading and affecting the price (Lambda>1) or to a separate profit accumulator (Lambda=1)
	if ($Lambda > 1)
		$add_net_balance_without_changing_price($balances, $y_in ? 'x' : 'y', $fee, $Lambda);

	return {
		net_amount_X: $net_amount_X,
		amount_Y: $amount_Y,
		fee: $fee,
		change: $change,
	}
};


const $swap = ($balances, $l_balances, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $pool_props) => {
	if ($delta_Yn)
		return $swap_by_delta_y($balances, $l_balances, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $pool_props);
	else if ($in_final_P)
		return $swap_by_final_p($balances, $l_balances, $x0, $y0, $y_in, $delta_Yn, $in_final_P, $received_amount_Y, $min_amount_out, $pool_props);
	else
		throw Error("unclear swap type")
}

const $buy_shares = ($s, $balances, $x0, $y0, $received_amount_x, $received_amount_y, $profits, $pool_props) => {
	
	const $Lambda = $pool_props.Lambda;
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	
//	$get_shares = ($x_balance, $y_balance) => round($x_balance**$alpha * $y_balance**$beta);
	const $get_shares = ($x_balance, $y_balance) => round(($x_balance/$y_balance)**$alpha * $y_balance);

	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;
	
	if (!$s){
		require_cond($received_amount_x > 0 && $received_amount_y > 0, "send both assets for the first issue");
		let $shares_amount;
		const $mid_price = $pool_props.mid_price;
		if ($mid_price){
			// first issue must be at mid price
			require_cond($received_amount_y === round($mid_price * $received_amount_x), "first issue must be at mid price "+$mid_price);
			const $gamma = $pool_props.gamma;
			$shares_amount = round($received_amount_x * $pool_props.mid_price_beta * $gamma / ($gamma - 1));
		}
		else{
			// first issue determines the price
			$shares_amount = $get_shares($received_amount_x, $received_amount_y);
		}
		$balances.xn = $balances.xn + $received_amount_x;
		$balances.yn = $balances.yn + $received_amount_y;
		$balances.x = $balances.x + round($received_amount_x * $Lambda);
		$balances.y = $balances.y + round($received_amount_y * $Lambda);
		return {
			shares_amount: $shares_amount,
			coef: 1,
			change_x: 0,
			change_y: 0,
		};
	}

	const $p = $alpha/$beta * ($y + $y0) / ($x + $x0);
	const $share_price_in_y = ($yn + $p * $xn) / $s;
	const $share_price_in_x = ($xn + 1 / $p * $yn) / $s;
	
	let $shares1, $delta_xn1, $delta_yn1;

	if ($Lambda > 1){
		const $proportional_y = round($yn/$xn * $received_amount_x);
		if ($received_amount_y > $proportional_y){ // ok if x is underleveraged
			const $target_xn = ceil($x/$Lambda);
			if ($xn > $target_xn){ // x is underleveraged
				const $max_delta_yn = round(($xn/$target_xn-1)*$yn);
				$delta_yn1 = min($max_delta_yn, $received_amount_y);
				$shares1 = floor($delta_yn1/$share_price_in_y);
			}
			$delta_xn1 = 0;
		}
		else{ // received x >= proportional x
			$target_yn = ceil($y/$Lambda);
			if ($yn > $target_yn){ // y is underleveraged
				$max_delta_xn = round(($yn/$target_yn-1)*$xn);
				$delta_xn1 = min($max_delta_xn, $received_amount_x);
				$shares1 = floor($delta_xn1/$share_price_in_x);
			}
			$delta_yn1 = 0;
		}
		$balances.xn = $balances.xn + $delta_xn1;
		$balances.yn = $balances.yn + $delta_yn1;
	}
	else{
		$delta_xn1 = 0;
		$delta_yn1 = 0;
		$shares1 = 0;
	}
	let $remaining = {
		x: $received_amount_x - $delta_xn1,
		y: $received_amount_y - $delta_yn1,
	};

	const $y_to_x = $balances.yn/$balances.xn;

	if ($profits.x || $profits.y){
		require_cond($Lambda === 1, "have profits while Lambda is " + $Lambda);

		// move proportional amounts of profit from both x and y. The shares to be issued for the moved profit belong to the pool and will not be actually issued
		let $delta_profit_x, $delta_profit_y, $symmetric_moved_profit_shares;
		const $profits_proportional_y = round($y_to_x * $profits.x);
		if ($profits.y > $profits_proportional_y){
			$delta_profit_x = $profits.x;
			$delta_profit_y = $profits_proportional_y;
			$symmetric_moved_profit_shares = $delta_profit_x/$xn * $s;
		}
		else{
			const $profits_proportional_x = round($profits.y / $y_to_x);
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

		// calc the shares to be issued for moving the one-sided profits to the pool, these shares belong to the pool and will not be actually issued
		const $moved_profit_x = min($profits.x, round($remaining.y / $y_to_x));
		const $moved_profit_y = min($profits.y, round($remaining.x * $y_to_x));

		const $moved_profit_shares = $moved_profit_x/$share_price_in_x + $moved_profit_y/$share_price_in_y + $symmetric_moved_profit_shares;
		log('share_price_in_x', $share_price_in_x, 'share_price_in_y', $share_price_in_y, 'moved_profit_shares', $moved_profit_shares, 'moved_profit_x', $moved_profit_x, 'moved_profit_y', $moved_profit_y);
		
		$profits.x = $profits.x - $moved_profit_x;
		$profits.y = $profits.y - $moved_profit_y;
		
		$remaining.x = $remaining.x + $moved_profit_x;
		$remaining.y = $remaining.y + $moved_profit_y;

	}

	// part 2: proportional buying
	log('before proportional buying: remaining', $remaining, 'y_to_x', $y_to_x);
	let $proportional_delta_xn, $proportional_delta_yn, $change_x, $change_y, $shares_proportional;
	const $remaining_proportional_y = round($y_to_x * $remaining.x);
	if ($remaining.y > $remaining_proportional_y){ // excessive y
		$proportional_delta_xn = $remaining.x;
		$proportional_delta_yn = $remaining_proportional_y;
		$change_x = 0;
		$change_y = $remaining.y - $remaining_proportional_y;
		$shares_proportional = ($remaining.x / $xn * $s);
	}
	else{ // excessive x
		const $remaining_proportional_x = round($remaining.y / $y_to_x);
		$proportional_delta_xn = $remaining_proportional_x;
		$proportional_delta_yn = $remaining.y;
		$change_x = $remaining.x - $remaining_proportional_x;
		log({proportional_delta_xn:$proportional_delta_xn, change_x:$change_x, remaining_x:$remaining.x, remaining_proportional_x:$remaining_proportional_x});
		require_cond($change_x >= 0, "received x " + $remaining.x + ", proportional " + $remaining_proportional_x);
		$change_y = 0;
		$shares_proportional = ($remaining.y / $yn * $s);
	}

	const $gross_shares_amount = $shares1 + $symmetric_moved_profit_shares + $shares_proportional;
	const $shares_amount = floor($gross_shares_amount - $moved_profit_shares);
	const $coef = ($s + $gross_shares_amount) / ($s + $shares_amount);
	log({shares_proportional:$shares_proportional, moved_profit_shares:$moved_profit_shares, shares_amount:$shares_amount});

	$balances.xn = $balances.xn + $proportional_delta_xn;
	$balances.yn = $balances.yn + $proportional_delta_yn;
	$balances.x = $balances.x + round($proportional_delta_xn * $Lambda);
	$balances.y = $balances.y + round($proportional_delta_yn * $Lambda);

	return {
		shares_amount: $shares_amount,
		coef: $coef,
		change_x: $change_x,
		change_y: $change_y,
	}

};


const $redeem_shares = ($s, $balances, $l_balances, $x0, $y0, $received_shares_amount, $asset, $pool_props) => {
	const $xn = $balances.xn;
	const $yn = $balances.yn;
	const $x = $balances.x;
	const $y = $balances.y;
	const $net_of_exit_fee = 1 - $pool_props.exit_fee;
	const $x_asset = $pool_props.x_asset;
	const $y_asset = $pool_props.y_asset;
	const $Lambda = $pool_props.Lambda;
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	let $xn_amount1, $yn_amount1, $remaining_received_shares;
	if ($asset){ // one-sided redemption first, then proportional
		require_cond($asset === $x_asset || $asset === $y_asset, "wrong preferred asset");
		require_cond($Lambda > 1, "only proportional withdrawals");
		const $asset_label = $asset === $x_asset ? 'x' : 'y';
		const $net_balance = $asset === $x_asset ? $xn : $yn;
		const $effective_balance = $asset === $x_asset ? $x : $y;
		const $target_net_balance = ceil($effective_balance / $Lambda);
	//	require($target_net_balance < $net_balance, "the preferred asset is already fully leveraged");
		const $excess_net_balance = $net_balance - $target_net_balance;
		const $p = $alpha/$beta * $y / $x;
		const $share_price_in_asset = ($asset_label === 'y') ? ($yn + $p * $xn) / $s : ($xn + 1/$p * $yn) / $s;
		const $max_asset = ceil($received_shares_amount * $share_price_in_asset);
		const $one_sided_amount = min($max_asset, $excess_net_balance);
		if ($asset_label === 'y'){
			$yn_amount1 = $one_sided_amount;
			$xn_amount1 = 0;
		}
		else{
			$xn_amount1 = $one_sided_amount;
			$yn_amount1 = 0;
		}
		$remaining_received_shares = max($received_shares_amount - ceil($excess_net_balance / $share_price_in_asset), 0);
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
	const $x_amount = floor($share_of_assets * $x * $net_of_exit_fee);
	const $y_amount = floor($share_of_assets * $y * $net_of_exit_fee);
	const $xn_amount = floor(($share_of_assets * ($xn - $xn_amount1) + $xn_amount1) * $net_of_exit_fee);
	const $yn_amount = floor(($share_of_assets * ($yn - $yn_amount1) + $yn_amount1) * $net_of_exit_fee);

	$balances.x = $balances.x - $x_amount;
	$balances.y = $balances.y - $y_amount;
	$balances.xn = $balances.xn - $xn_amount;
	$balances.yn = $balances.yn - $yn_amount;

	const $new_x0 = $x0 * ($s-$received_shares_amount)/$s;
	const $new_y0 = $y0 * ($s-$received_shares_amount)/$s;
	const $denom = 1 - $get_utilization_ratio($balances, $l_balances, $new_x0, $new_y0, $alpha);
	require_cond($denom >= $singularity_threshold, "redemption amount too large, it would bring us too close to the singularity point, denom="+$denom);

	return {
		xn_amount: $xn_amount,
		yn_amount: $yn_amount,
		x_amount: $x_amount,
		y_amount: $y_amount,
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
		PL1_ratio: $pow($ratio_powers, $Leverage) / $ratio,
	};
	$get_leverages().forEach($L => {
		const $allyL = $inverted ? -$L : $L;
		const $balance = $l_balances[$allyL+'x']?.balance;
		const $obalance = $l_balances[-$allyL+'x']?.balance;
		if (!$balance && !$obalance)
			return;
	//	log({$L, $balance, $obalance, $ratio})
		const $ratio_L1 = $pow($ratio_powers, $L) / $ratio;
		if ($balance){
			$sums.initial = $sums.initial + $balance * ($L-1)/$L;
			if ($L !== $Leverage){
				const $new_balance = ($balance * $ratio_L1);
				$l_balances[$allyL+'x'].balance = $new_balance;
				$sums.final = $sums.final + $new_balance * ($L-1)/$L;
				$sums.XL_denom = $sums.XL_denom + $new_balance * ($L-1);
				$sums.delta_XL = $sums.delta_XL + $new_balance - $balance;
			}
		}
		if ($obalance){
			$sums.initial = $sums.initial - $obalance/$P;
			const $new_obalance = ($obalance / $ratio_L1);
			$l_balances[-$allyL+'x'].balance = $new_obalance;
			$sums.final = $sums.final - $new_obalance/$final_P;
			$sums.YL_denom = $sums.YL_denom + $new_obalance * ($L-1);
			$sums.delta_XL = $sums.delta_XL - ($new_obalance / $final_P - $obalance / $P) * ($L-1)/$L; // delta of the swap-pool's X: borrowed X changes by trading in the pool
		}
	});
//	$sums.delta_XL = round($sums.delta_XL);
	return $sums
};


// delta_Xn < 0: buy L-tokens
// delta_Xn > 0: sell L-tokens
const $trade_l_shares = ($balances, $l_balances, $x0, $y0, $Leverage, $asset, $delta_Xn, $entry_price, $pool_props) => {
	require_cond(is_integer($delta_Xn), "delta must be int");
	require_cond($asset === $pool_props.x_asset || $asset === $pool_props.y_asset, "wrong asset");
	require_cond($Leverage === 2 || $Leverage === 5 || $Leverage === 10 || $Leverage === 20 || $Leverage === 50 || $Leverage === 100, "bad L");
	const $Lambda = $pool_props.Lambda;

	let $inverted, $X, $Y, $Xn, $Yn, $X0, $Y0, $a, $b;

	if ($asset === $pool_props.x_asset){
		$inverted = false;
		$X = $balances.x;
		$Y = $balances.y;
		$Xn = $balances.xn;
		$Yn = $balances.yn;
		$X0 = $x0;
		$Y0 = $y0;
		$a = $pool_props.alpha;
		$b = $pool_props.beta;
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
	}
	require_cond($Xn + $delta_Xn > 0, "Xn balance would be negative");
	const $L_key = ($inverted ? -$Leverage : $Leverage) + 'x';
	const $l_bal_direction = $delta_Xn < 0 ? "grow" : "fall";
	let $pool = {X: $X, Y: $Y, Xn: $Xn, Yn: $Yn, XL: 0, delta_XL: 0, };


	const $move_unleveraged = ($dXn) => {
		const $Xt = $pool.X + $X0;
		const $P = $a/$b * ($pool.Y + $Y0) / $Xt;
		const $final_Xn = $pool.Xn + $dXn;
		const $final_X = $final_Xn;
		const $final_Xt = $final_X + $X0;
		const $final_Y = $get_final_y($pool.X, $pool.Y, $final_X, $X0, $Y0, $pool_props, $inverted);
		const $delta_Y = $final_Y - $pool.Y;
		const $delta_Yn = $delta_Y;
		const $final_Yn = $pool.Yn + $delta_Yn;
		const $final_P = $a/$b * ($final_Y + $Y0) / $final_Xt;

		log('l balances before', $l_balances);
		const $sums = $update_other_l_balances_and_get_sums($l_balances, $P, $final_P, $Leverage, $inverted);
		log('sums', $sums);
		log('l balances after', $l_balances);

		// update the final balance of our L-pool
		const $b1 = $sums.initial + $b/($b-1)*$Xt;
		const $new_l_balance = ( $Leverage/($Leverage-1) * ( -$sums.final - $b/($b-1)*$final_Xt + $b1 * ($final_Xt/$Xt)**(1/$b) ) );
		log('new_l_balance', $new_l_balance);
		const $delta_l_balance = $new_l_balance - $l_balances[$L_key].balance;
		log('delta_l_balance', $delta_l_balance);
		require_cond($delta_l_balance * $delta_Xn < 0, "unleveraged l-bal should "+$l_bal_direction+", got new " + $new_l_balance + ", delta " + $delta_l_balance);
		$l_balances[$L_key].balance = $new_l_balance;
		log('l balances after 2', $l_balances);

		$pool.X = $final_X;
		$pool.Y = $final_Y;
		$pool.Xn = $final_Xn;
		$pool.Yn = $final_Yn;
		$pool.delta_XL = $pool.delta_XL + $sums.delta_XL;
		$pool.XL_denom = $sums.XL_denom + $new_l_balance * ($Leverage-1);
		$pool.YL_denom = $sums.YL_denom;
		$pool.PL1_ratio = $sums.PL1_ratio;
	};

	const $move_along_X = ($dXn) => {
		const $P = $a/$b * ($pool.Y + $Y0) / ($pool.X + $X0);
		const $final_Xn = $pool.Xn + $dXn;
		const $final_X = $Lambda * $final_Xn;
		const $final_Y = $get_final_y_along_x($pool.X, $pool.Y, $final_X, $pool_props, $inverted);
		const $delta_Y = $final_Y - $pool.Y;
		const $delta_Yn = -round($a/($b*$Lambda-1)*$delta_Y);
		const $final_Yn = $pool.Yn + $delta_Yn;
		const $final_P = $a/$b * ($final_Y + $Y0) / ($final_X + $X0);

		const $sums = $update_other_l_balances_and_get_sums($l_balances, $P, $final_P, $Leverage, $inverted);

		// update the final balance of our L-pool
		const $b1 = $sums.initial + $b/($b*$Lambda-1)*$pool.X;
		const $new_l_balance = round( $Leverage/($Leverage-1) * ( -$sums.final - $b/($b*$Lambda-1)*$final_X + $b1 * ($final_X/$pool.X)**(1/$b/$Lambda) ) );
		const $delta_l_balance = $new_l_balance - $l_balances[$L_key].balance;
		require_cond($delta_l_balance * $delta_Xn < 0, "along x l-bal should "+$l_bal_direction+", got new " + $new_l_balance + ", delta " + $delta_l_balance);
		$l_balances[$L_key].balance = $new_l_balance;

		$pool.X = $final_X;
		$pool.Y = $final_Y;
		$pool.Xn = $final_Xn;
		$pool.Yn = $final_Yn;
		$pool.delta_XL = $pool.delta_XL + $sums.delta_XL;
		$pool.XL_denom = $sums.XL_denom + $new_l_balance * ($Leverage-1);
		$pool.YL_denom = $sums.YL_denom;
		$pool.PL1_ratio = $sums.PL1_ratio;
	};

	const $move_along_Y = ($dXn) => {
		const $P = $a/$b * ($pool.Y + $Y0) / ($pool.X + $X0);
		const $final_Xn = $pool.Xn + $dXn;
		const $delta_X = -round(($a*$Lambda-1)/$b * $dXn);
		const $final_X = $pool.X + $delta_X;
		const $final_Y = $get_final_y_along_y($pool.X, $pool.Y, $final_X, $pool_props, $inverted);
		const $final_Yn = round($final_Y/$Lambda);
		const $final_P = $a/$b * ($final_Y + $Y0) / ($final_X + $X0);

		const $sums = $update_other_l_balances_and_get_sums($l_balances, $P, $final_P, $Leverage, $inverted);

		// update the final balance of our L-pool
		const $b2 = $sums.initial - $b/$a/$Lambda*$pool.X;
		const $new_l_balance = round( $Leverage/($Leverage-1) * ( -$sums.final + $b/$a/$Lambda*$final_X + $b2 * ($final_X/$pool.X)**(-1/($a*$Lambda-1)) ) );
		const $delta_l_balance = $new_l_balance - $l_balances[$L_key].balance;
		require_cond($delta_l_balance * $delta_Xn < 0, "along y l-bal should "+$l_bal_direction+", got new " + $new_l_balance + ", delta " + $delta_l_balance);
		$l_balances[$L_key].balance = $new_l_balance;

		$pool.X = $final_X;
		$pool.Y = $final_Y;
		$pool.Xn = $final_Xn;
		$pool.Yn = $final_Yn;
		$pool.delta_XL = $pool.delta_XL + $sums.delta_XL;
		$pool.XL_denom = $sums.XL_denom + $new_l_balance * ($Leverage-1);
		$pool.YL_denom = $sums.YL_denom;
		$pool.PL1_ratio = $sums.PL1_ratio;
	};

	if (!$l_balances[$L_key])
		$l_balances[$L_key] = { balance: 0, supply: 0 };
	const $initial_l_balance = $l_balances[$L_key].balance;
	const $initial_shares = $l_balances[$L_key].supply;
	require_cond(!$initial_l_balance === !$initial_shares, "l balance "+$initial_l_balance+" while shares "+$initial_shares);
	const $initial_P = $a/$b * ($pool.Y + $Y0) / ($pool.X + $X0);

	if ($Lambda === 1)
		$move_unleveraged($delta_Xn);
	else {
		const $underleveraged = $Xn > ceil($X/$Lambda);
		if (!$underleveraged){ // along X
			if ($delta_Xn > 0){ // selling L-shares and X
				var $delta_Xn_inflection = round($X * (( $Lambda/($Lambda-1) * ($a + ($b * $Lambda - 1) * $Yn/$Y) )**($b * $Lambda/($b*$Lambda-1)) - 1) / $Lambda);
				var $inflected = $delta_Xn > $delta_Xn_inflection;
			}
			const $dXn1 = $inflected ? $delta_Xn_inflection : $delta_Xn;
			$move_along_X($dXn1);
			if ($inflected)
				$move_along_Y($delta_Xn - $delta_Xn_inflection);
		}
		else{ // along Y
			if ($delta_Xn < 0){ // buying L-shares and X
				var $delta_Xn_inflection = -round($b/($Lambda-1) * ($Lambda*$Xn - $X));
				var $inflected = abs($delta_Xn) > abs($delta_Xn_inflection);
			}
			const $dXn1 = $inflected ? $delta_Xn_inflection : $delta_Xn;
			$move_along_Y($dXn1);
			if ($inflected)
				$move_along_X($delta_Xn - $delta_Xn_inflection);
		}
	}

	const $final_l_balance = $l_balances[$L_key].balance;
	const $delta_l_balance = $final_l_balance - $initial_l_balance;

	// the change in the total X balance of swap pool + all L-pools (positive when buying, negative when selling)
	const $net_delta = round($delta_Xn + $pool.delta_XL + $delta_l_balance);
	log({$delta_Xn, delta_XL:$pool.delta_XL, $delta_l_balance})

	const $final_shares = $initial_l_balance
		? round($final_l_balance/$initial_l_balance / $pool.PL1_ratio * $initial_shares)
		: $net_delta; // the initial units for shares are arbitrary
	const $shares = $final_shares - $initial_shares;
	$l_balances[$L_key].supply = $final_shares;
	const $avg_share_price = $net_delta/$shares;

	const $denom = 1 - $pool.XL_denom/$b/($pool.X+$X0) - $pool.YL_denom/$a/($pool.Y+$Y0);
	log('denom after L', $denom);
	require_cond($denom >= $singularity_threshold, "too close to the singularity point, denom="+$denom+", need more liquidity in order to buy this amount of L-tokens");

	$balances.x = $inverted ? $pool.Y : $pool.X;
	$balances.y = $inverted ? $pool.X : $pool.Y;
	$balances.xn = $inverted ? $pool.Yn : $pool.Xn;
	$balances.yn = $inverted ? $pool.Xn : $pool.Yn;

	// regular trading fee (%) and arb tax are paid on top
	const $final_P = $a/$b * ($pool.Y + $Y0) / ($pool.X + $X0);
	const $arb_profit_in_Y = ($final_P - $initial_P) * $net_delta / 2; // in Y
//	log({$final_P, $initial_P, $net_delta})
	require_cond($arb_profit_in_Y > 0, "arb profit "+$arb_profit_in_Y);
	const $arb_profit_in_X = $arb_profit_in_Y / $final_P;
	const $trading_fee = ceil($arb_profit_in_X * $pool_props.arb_profit_tax + abs($net_delta) * $pool_props.swap_fee);

	let $l_tax = 0;
	if ($delta_Xn > 0){ // sell
		const $gross_asset_out = -$net_delta; // gross means before tax and fees
		require_cond($gross_asset_out > 0, "asset out must be positive, got " + $gross_asset_out);
	//	log('-selling: ', { $entry_price, $avg_share_price, $shares, $pool_props });
		if ($entry_price){
			// L>0 profit is accounted for in x tokens (in y tokens for L<0) meaning that only profits from the borrowed part of the L-pool are taxed
			$l_tax = max(ceil(($avg_share_price - $entry_price)*(-$shares)*$pool_props.leverage_profit_tax), 0);
		}
		else
			$l_tax = ceil($gross_asset_out * $pool_props.leverage_token_tax);
	}
	const $total_fee = $trading_fee + $l_tax;
	if ($Lambda > 1)
		$add_net_balance_without_changing_price($balances, $inverted ? 'y' : 'x', $total_fee, $Lambda);
	// For buying, the fee is added on top. For selling (net_delta<0), the fees are subtracted
	const $gross_delta = $net_delta + $total_fee;

	return {
		shares: $shares,
		net_delta: $net_delta,
		gross_delta: $gross_delta,
		avg_share_price: $avg_share_price,
		l_tax: $l_tax,
		trading_fee: $trading_fee,
		total_fee: $total_fee,
	}
};


const $handle_trade_l_shares_request = ($pool_vars, $balances, $l_balances, $x0, $y0, $trigger_data, $trigger_address, $trigger_outputs, $pool_props) => {
	const $x_asset = $pool_props.x_asset;
	const $y_asset = $pool_props.y_asset;

	const $asset = $trigger_data.asset === 'x' ? $x_asset : ($trigger_data.asset === 'y' ? $y_asset : $trigger_data.asset);
	const $L = $trigger_data.L;
	const $buy = $trigger_data.buy;
	const $sell = $trigger_data.sell;
	const $delta = $trigger_data.delta;
	const $received_amount = $trigger_outputs[$asset];
	const $min_received_amount = $asset === 'base' ? 10000 : 0;
	const $net_received_amount = $received_amount - $min_received_amount;
	
	require_cond(!($buy && $sell), "buy or sell?");
	require_cond($delta > 0, "delta must be positive");
	if ($buy)
		require_cond($net_received_amount > 0, "you forgot to pay");
	else
		require_cond($net_received_amount === 0, "don't send asset");
	const $delta_Xn = $buy ? -$delta : $delta; // Xn in the pool
	const $asset_label = $asset === $x_asset ? 'x' : 'y';

	const $signedL = $asset_label === 'x' ? $L : -$L;
	if ($buy && $trigger_data.tokens || $sell && !$trigger_data.position){
		var $l_shares_asset = $pool_vars['leveraged_asset' + $signedL];
		require_cond($l_shares_asset, "please define an asset for the leveraged token first");
	}
	if ($sell){
		if ($trigger_data.position){
			var $position = $pool_vars[$trigger_data.position];
			require_cond($position, "no such position");
			require_cond($position.owner === $trigger_address, "you are not the owner of this position");
			const $parts = split($trigger_data.position, '_');
			require_cond(+$parts[1] === $signedL, "wrong L");
			var $shares_in = $position.shares;
		}
		else{
			var $shares_in = $trigger_outputs[$l_shares_asset];
			require_cond($shares_in > 0, "no leveraged tokens received");
		}
	}

	const $res = $trade_l_shares($balances, $l_balances, $x0, $y0, $L, $asset, $delta_Xn, $position.price, $pool_props);

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



const $validate_and_apply_new_governed_param_value = ($name, $value, $balances, $profits, $lp_shares, $pool_props, $locked_governance) => {
	const $Lambda = $pool_props.Lambda;
	const $alpha = $pool_props.alpha;
	const $beta = $pool_props.beta;
	const $gamma = $pool_props.gamma;
	const $mid_price = $pool_props.mid_price;

	if ($locked_governance)
		require_cond(!$locked_governance[$name], "governance is not allowed to change "+$name);
	
	if ($name === 'pool_leverage'){
		require_cond(!$profits.x && !$profits.y, "profits must be added to the pool before changing pool_leverage");
		require_cond($alpha !== 1/$value, "pool leverage = 1/alpha");
		require_cond($beta !== 1/$value, "pool leverage = 1/beta");
		if ($value > 1){
			require_cond(!$mid_price, "price range setting is incompatible with new pool leverage");
			$balances.x = round($balances.x * $value/$Lambda);
			$balances.y = round($balances.y * $value/$Lambda);
		}
		else{
			$balances.x = $balances.xn;
			$balances.y = $balances.yn;
		}
		// we modified balances
	}
	else if ($name === 'mid_price' || $name === 'price_deviation'){
		require_cond($value && $mid_price, $name+" must be nonzero");
		require_cond($alpha === 0.5, "equal weights only");
		let $new_p0, $new_gamma;
		if ($name === 'price_deviation'){
			require_cond($value > 1, "price deviation must be > 1");
			$new_p0 = $mid_price;
			$new_gamma = $value;
		}
		else{
			$new_p0 = $value;
			$new_gamma = $gamma;
		}
		const $s_curve = $lp_shares.linear * $lp_shares.coef;
		const $sqp = sqrt($new_p0);
		const $b = ($balances.x/$s_curve*$sqp + $balances.y/$s_curve/$sqp)/$new_gamma;
		const $a = $balances.x*$balances.y/$s_curve/$s_curve;
		$lp_shares.coef = $lp_shares.coef / (-$b + sqrt($b*$b - 4*$a*(1/$new_gamma/$new_gamma-1)))*2*$a;
		// we modified lp_shares
	}
	else if ($name === 'alpha'){
		require_cond(!$mid_price, "can't change token weights while trading in limited range");
		require_cond($value !== 1/$Lambda && 1-$value !== 1/$Lambda, "pool leverage = 1/alpha or 1/beta");
		// s_coef is unused
	//	var['s_coef'] *= $balances.xn**$value * $balances.yn**(1-$value) / $s_curve;
		// nothing modified
	}
};

function getPoolState(aaParams, stateVars) {
	const { balances, leveraged_balances, profits, lp_shares } = stateVars;
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
		base_interest_rate: getParam('base_interest_rate', 0.2),
		x_asset,
		y_asset,
	};
	const last_ts = stateVars.last_ts;
	return { balances, leveraged_balances, profits, lp_shares, pool_props, shifts, bounds, last_ts };
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
	const { balances, leveraged_balances, profits, shifts: { x0, y0 }, pool_props, last_ts } = poolState;
	const { alpha, Lambda } = pool_props;
	const i = getInterestRate(poolState);
	$charge_interest(balances, leveraged_balances, profits, x0, y0, last_ts, i, alpha, Lambda);
}

function getSwapParams(in_amount, in_token, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, shifts: { x0, y0 }, pool_props } = poolState;

	in_token = toXY(in_token, pool_props);
	const y_in = in_token === 'y';
	
	const { res, required_amount, param_value } = findParamToMatchAmount(in_amount, delta_Yn => {
		const res = $swap(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), x0, y0, y_in, delta_Yn, 0, -1, 0, pool_props);
		return { res, required_amount: res.amount_Y };
	});
	return { res, delta_Yn: param_value };
}

function getLeveragedBuyParams(in_amount, in_token, leverage, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, shifts: { x0, y0 }, pool_props } = poolState;

	in_token = toAsset(in_token, pool_props);

	const { res, required_amount, param_value } = findParamToMatchAmount(in_amount, delta => {
		const res = $trade_l_shares(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), x0, y0, leverage, in_token, -delta, 0, pool_props);
		return { res, required_amount: res.gross_delta };
	});
	return { res, delta: param_value };
}

function getLeveragedSellParams(in_amount, token, leverage, entry_price, poolState) {
	poolState = _.cloneDeep(poolState); // make a copy so that we don't modify the input params
	chargeInterest(poolState);

	const { balances, leveraged_balances, shifts: { x0, y0 }, pool_props } = poolState;

	token = toAsset(token, pool_props);

	const { res, required_amount, param_value } = findParamToMatchAmount(in_amount, delta => {
		const res = $trade_l_shares(_.cloneDeep(balances), _.cloneDeep(leveraged_balances), x0, y0, leverage, token, delta, entry_price, pool_props);
		return { res, required_amount: -res.shares };
	});
	return { res, delta: param_value };
}

function findParamToMatchAmount(target_amount, f) {
	
	let param_value = target_amount; // initial estimation

	let prev_param_value;
	let prev_distance = Infinity;
	let prev_slope;
	let prev_required_amount;
	let count = 0;
	while (true) {
		count++;
		log(`${count}: trying value ${param_value}`);
		const { required_amount, res } = f(param_value);
		const distance = target_amount - required_amount;
		const approach = prev_distance / distance;
		const delta_param_value = param_value - prev_param_value;

		// 1st derivative
		let slope = prev_param_value ? (param_value - prev_param_value) / (required_amount - prev_required_amount) : param_value / required_amount;
		if (distance < 1000) // too noisy probably due to rounding
			slope = param_value / required_amount;
		
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

		if (prev_slope) { // 2nd term of Taylor series
			const second_derivative = (slope - prev_slope) / (required_amount - prev_required_amount);
			param_value += 1 / 2 * second_derivative * (target_amount - required_amount) ** 2;
			log('2nd derivative', 1 / 2 * second_derivative * (target_amount - required_amount) ** 2)
		}
		param_value = round(param_value);
		
		prev_distance = distance;
		prev_slope = prev_required_amount && slope;
		prev_required_amount = required_amount;
		if (count > 100)
			throw Error(`too many iterations, target ${target_amount}, last ${required_amount}`)
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
exports.getLeveragedBuyParams = getLeveragedBuyParams;
exports.getLeveragedSellParams = getLeveragedSellParams;

