# Backtest Observations

## Baseline (s1 original)
- **Median Win/Loss Ratio**: 1.50425
- **Total Profit**: 0.41534
- **Total Orders**: 2879
- **Median Profit**: 0.07230
- **Median Loss**: -0.00157

Configuration:
- Trend: HMA(25)
- Momentum: STOCH(14) (83/23)
- Volatility: ATR(10), SMA(20) smoothing
- Rules: TrendActiveFor(1h), VolatilityIsHigh, Momentum checks.

Observations:
- High number of orders suggests it's catching a lot of noise.
- Win/Loss ratio is decent but can be improved.
- USDCAD performing poorly.
