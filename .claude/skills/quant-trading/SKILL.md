---
name: quant-trading
description: Quantitative trading strategy development, backtesting, and risk management
schema: "1.0"
version: 1.1.0
triggers:
  keywords:
    primary:
      - quant
      - trading
      - quantitative
      - trade
      - backtest
      - backtesting
      - algo-trading
      - algorithmic trading
    secondary:
      - strategy
      - strategy design
      - factor
      - factor model
      - arbitrage
      - arbitrage trading
      - hedge
      - hedging
  context_boost:
    - python
    - pandas
    - numpy
    - finance
    - investment
    - stock
    - futures
  context_penalty:
    - design
    - marketing
    - frontend
  priority: high
keywords:
  - finance
  - trading
  - quantitative
  - investment
dependencies:
  software-skills:
    - python
    - database
    - data-analysis
author: claude-domain-skills
metadata:
  mcpmarket-version: 1.0.0
---
# Quant Trading

> Systematic, data-driven trading strategy development

## When to Use

- Developing trading strategies (trend following, mean reversion, arbitrage)
- Strategy backtesting and performance analysis
- Risk management and position control
- Factor research and alpha discovery

## Strategy Development Process

```
Hypothesis → Data prep → Strategy coding → Backtest validation → Risk control → Live monitoring
```

## Core Knowledge

### Strategy Types

| Type | Description | Risk |
|------|------|------|
| **Trend following** | Go with the trend, buy strength / sell weakness | Loses in choppy markets |
| **Mean reversion** | Revert after price deviates | Loses in trending markets |
| **Statistical arbitrage** | Pairs trading, spread convergence | Correlation breakdown |
| **High-frequency trading** | Capturing spreads at microsecond scale | High technical risk |

### Risk Metrics

| Metric | Formula | Healthy Standard |
|------|------|----------|
| **Sharpe ratio** | (Return - risk-free) / std dev | > 1.5 |
| **Maximum drawdown** | Largest peak-to-trough decline | < 20% |
| **Calmar ratio** | Annualized return / max drawdown | > 1.0 |
| **Win rate** | Winning trades / total trades | > 50% |

### Common Factors

| Factor | Description | Rationale |
|------|------|------|
| **Value** | Low P/E, P/B | Cheap stocks outperform long term |
| **Momentum** | Past winners keep winning | Trend persistence |
| **Quality** | High ROE, low debt | Premium for good companies |
| **Size** | Small-cap premium | Liquidity compensation |
| **Volatility** | Low-volatility anomaly | Low risk, high return |

### Backtest Checklist

- **Data quality**: Is there survivorship bias?
- **Look-ahead bias**: Are you using future data?
- **Overfitting**: Are the parameters over-optimized?
- **Trading costs**: Are commissions and slippage included?
- **Out-of-sample testing**: Did you hold out a test set?

## Best Practices

1. **Simple before complex** - Start with a simple strategy, add complexity gradually
2. **Out-of-sample validation** - Always hold out a slice of data for the final test
3. **Account for trading costs** - Add realistic commissions and slippage in backtests
4. **Diversify risk** - Don't bet all your capital on a single strategy
5. **Monitor continuously** - Track live performance and set stop-loss conditions

## Common Mistakes

| Mistake | Correct Approach |
|------|----------|
| Going live because backtest returns look amazing | Check for overfitting |
| Ignoring trading costs | Add realistic commissions and slippage |
| Training on all the data | Split into train/validation/test sets |
| All-in on a single strategy | Diversify with a multi-strategy portfolio |

## Sharp Edges

### SE-1: Backtest Overfitting
- **Severity**: critical
- **Scenario**: Strategy performs superbly in backtest (Sharpe > 3) but loses money live
- **Cause**: Parameters over-optimized to historical data, capturing noise instead of signal
- **Symptoms**: Backtest Sharpe > 3, live performance < 50%, parameter-sensitive, too few trades
- **Detection**: `sharpe.*[3-9]\.|sharpe.*\d{2,}`
- **Fix**: Use walk-forward validation, rolling tests across 5+ segments

### SE-2: Look-ahead Bias
- **Severity**: critical
- **Scenario**: Inadvertently using future data in the backtest
- **Cause**: Time ordering not respected during data processing
- **Symptoms**: Perfect backtest returns, using same-day close as the same-day signal
- **Detection**: `shift\(-|iloc\[-1\].*today`
- **Fix**: Signals must be lagged by one period `signal = prices.shift(1) > ma.shift(1)`

### SE-3: Survivorship Bias
- **Severity**: high
- **Scenario**: Backtesting using only currently-existing stocks
- **Cause**: Delisted/removed stocks are excluded, overstating strategy performance
- **Symptoms**: Backtest returns clearly higher than reality, small-cap strategies look especially good
- **Fix**: Use a complete database that includes delisted stocks; use point-in-time data

### SE-4: Ignoring Trading Costs and Slippage
- **Severity**: high
- **Scenario**: High-turnover strategy looks very profitable
- **Cause**: Commissions and slippage not counted; real costs eat all the profit
- **Symptoms**: Annual turnover > 1000%, per-trade profit < 0.5%, backtest fills at the close
- **Fix**: Include commission (0.1%) + slippage (0.2%) = 0.3% per trade

### SE-5: Overconfidence in In-Sample Performance
- **Severity**: medium
- **Scenario**: Deciding to go live based only on in-sample backtest results
- **Cause**: No independent test set was held out
- **Symptoms**: No out-of-sample test, validation set reused, test set too small
- **Fix**: Split data 60/20/20 (train/validation/test); use the test set only once

## Risk Management Framework

**Layer 1 – Trade level**: 2% per-trade stop-loss, set take-profit based on volatility, trailing stop to protect profit
**Layer 2 – Strategy level**: No single strategy over 20% of total capital, halve position when drawdown exceeds 15%
**Layer 3 – Portfolio level**: Daily VaR no more than 2%, stress testing, keep 20% cash

### Kelly Formula

```
f* = (p × b - q) / b
f* = optimal bet fraction, p = win rate, q = loss rate, b = odds
Example: win rate 55%, odds 1.5 → f* = 25% (in practice use Half-Kelly, 12.5%)
```

### Execution Risk

| Risk | Description | Countermeasure |
|------|------|------|
| **Slippage** | Fill price differs from expected | Limit orders, split execution |
| **Liquidity** | Can't fill enough size | Avoid small caps, set size caps |
| **System failure** | Program/network outage | Backup systems, manual monitoring |
| **Black swan** | Extreme events | Position caps, stop-loss mechanisms |

## Recommended Tools

- **Backtrader** - Python backtesting framework
- **QuantConnect** - Cloud quant platform
- **TradingView** - Chart analysis and strategy testing
- **Zipline** - Quantopian's open-source backtesting engine

## Further Resources

- Code examples: [extended/code-examples.md](extended/code-examples.md)
- Templates and checklists: [extended/templates.md](extended/templates.md)

## References

- Quantitative Trading - Ernest Chan
- Advances in Financial Machine Learning - Marcos Lopez de Prado

## Disclaimer

Quantitative trading involves high risk; past performance does not indicate future results. This content is for educational reference only and does not constitute investment advice.
