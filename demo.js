const { $swap, $trade_l_shares, getPoolState, getSwapParams, toAsset, getLeveragedBuyParams, getLeveragedSellParams, setLogger } = require('./index.js');

setLogger(console.log);


const balances = {
	x: 1e9,
	y: 100e9,
};
balances.xn = balances.x;
balances.yn = balances.y;

const leveraged_balances = {
	'-10x': { balance: 65086351.1657374, supply: 14119007 },
	'100x': { balance: 288.98010101, supply: 37894 },
	'5x': { balance: 9029669.3877427, supply: 2690494 }
};

const profits = {
	x: 0,
	y: 0,
};

const price = balances.yn / balances.xn;
const recent = {
	last_ts: Math.round(Date.now() / 1000) - 600, // 10 minutes ago
	prev: {
		start_ts: Math.round(Date.now() / 1000) - 1.5 * 3600, // 1.5 hour ago
		pmin: price,
		pmax: price,
	},
	current: {
		start_ts: Math.round(Date.now() / 1000) - 0.5 * 3600, // 0.5 hour ago
		pmin: price,
		pmax: price,
	},
	last_trade: {
		address: 'SOMEONE',
		pmin: price,
		pmax: price,
		amounts: { x: 200, y: 0 },
		paid_taxes: { x: 2, y: 0 },
	},
};

const params = {
	alpha: 0.5,
	swap_fee: 0.003,
	arb_profit_tax: 0.9,
	leverage_profit_tax: 0.2,
	leverage_token_tax: 0.1,
	x_asset: 'xass',
	y_asset: 'yass',
};
const stateVars = {
	balances,
	leveraged_balances,
	profits,
	recent,
};

const poolState = getPoolState(params, stateVars);
const { shifts: { x0, y0 }, pool_props } = poolState;

//const res = $swap(balances, leveraged_balances, x0, y0, true, 0, 110, -1, 0, pool_props);
//const res = $swap(balances, leveraged_balances, x0, y0, true, 1e9, 0, -1, 0, pool_props);
//console.log(res);

function swap(in_amount, in_token) {
	const swapParams = getSwapParams(in_amount, in_token, poolState);
	console.log(swapParams);
	const { delta_Yn, amount_Y } = swapParams;

	const res = $swap(balances, leveraged_balances, profits, recent, x0, y0, true, delta_Yn, 0, amount_Y, 0, 'ADDRESS', pool_props);
	console.log(`=== sold ${in_amount} ${in_token}`, res);
}

function buyLeverage(in_amount, in_token, leverage) {
	const leverageParams = getLeveragedBuyParams(in_amount, in_token, leverage, poolState);
	console.log('-- buy leverage params', leverageParams);
	const { delta } = leverageParams;

	const res = $trade_l_shares(balances, leveraged_balances, profits, recent, x0, y0, leverage, toAsset(in_token, pool_props), -delta, 0, 'ADDRESS', pool_props);
	console.log(`=== bought L-shares for ${in_amount} ${in_token}`, res, leveraged_balances);
	return res;
}

function sellLeverage(in_amount, token, leverage, entry_price) {
	const leverageParams = getLeveragedSellParams(in_amount, token, leverage, entry_price, poolState);
	console.log('-- sell leverage params', leverageParams);
	const { delta } = leverageParams;

	const res = $trade_l_shares(balances, leveraged_balances, profits, recent, x0, y0, leverage, toAsset(token, pool_props), delta, entry_price, 'ADDRESS', pool_props);
	console.log(`=== sold ${in_amount} L-shares`, res, leveraged_balances);
}

swap(1e9, 'y');
const { shares, avg_share_price } = buyLeverage(1.0e6, 'x', 10);
console.log(`\n==========================\n`);
sellLeverage(Math.round(shares/2), 'x', 10, avg_share_price);
