const _ = require('lodash');
const poolLib = require('./pool-lib.js');
const { $swap, $trade_l_shares, $redeem_shares, $charge_interest, $get_utilization_ratio } = poolLib;


function require_cond(cond, err) {
	if (!cond)
		throw Error(err);
}

let log = () => { }
function setLogger(logger) {
	poolLib.setLogger(logger);
	log = logger;
}




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
	const delta_Yn_initial_estimation = balances[in_token + 'n'] * out_amount / (balances[out_token + 'n'] - out_amount);
	
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
	//	if (required_amount === prev_required_amount) {
	//		log(`repeated amount ${required_amount}`);
	//		return { res, required_amount, param_value };
	//	}

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

