---
name: strategy-optimization
description: Trading strategy optimization methodology, an iterative process from diagnosis to hitting the target
schema: "1.0"
version: 1.0.0
triggers:
  keywords:
    primary:
      - strategy optimization
      - optimize strategy
      - improve win rate
      - improve returns
    secondary:
      - win rate
      - backtest
      - parameter tuning
      - performance improvement
    context_boost:
      - trading
      - quant
      - return
      - profit
  context_penalty:
    - design
    - marketing
    - frontend
  priority: high
keywords:
  - finance
  - trading
  - optimization
  - strategy
  - backtest
dependencies:
  domain-skills:
    - quant-trading
author: claude-domain-skills
metadata:
  mcpmarket-version: 1.0.0
---
# Trading Strategy Optimization

> A systematic strategy optimization process, from diagnosing the problem to reaching the goal

## When to Use

- Win rate too low (< 40%)
- Returns below target
    - Strategy performs poorly in specific market states
    - Need to hit a specific return target (e.g. 15%+)

## Core Process

```
┌─────────────────────────────────────────────────────────────────┐
  │                                                                 │
  │                   Strategy Optimization Iteration Loop          │
  │                                                                 │
  │  ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐         │
  │  │ ANALYZE │ → │ DIAGNOSE│ → │ RESEARCH│ → │IMPLEMENT│         │
  │  │         │   │         │   │         │   │         │         │
  │  └────┬────┘   └────┬────┘   └────┬────┘   └────┬────┘         │
  │       │             │             │             │               │
  │       ▼             ▼             ▼             ▼               │
  │  gather metrics  find root cause  search solutions  make changes │
  │                                                                 │
  │       ┌─────────────────────────────────────────┐               │
  │       │                                         │               │
  │       ▼                                         │               │
  │  ┌─────────┐   ┌─────────┐                     │               │
  │  │VALIDATE │ → │ ITERATE │ ────────────────────┘               │
  │  │         │   │         │   not met → back to ANALYZE          │
  │  └────┬────┘   └─────────┘                                      │
  │       │                                                         │
  │       ▼                                                         │
  │   target met → document & deploy                               │
  │                                                                 │
  └─────────────────────────────────────────────────────────────────┘
```

## Phase 1: ANALYZE

Gather current performance metrics:

| Metric | Description | Healthy Standard |
  |------|------|----------|
| **Win rate** | Proportion of winning trades | > 40% |
| **Return** | Total profit/loss | Positive |
| **Drawdown** | Maximum loss magnitude | < 20% |
| **Trade count** | Statistical significance | > 100 trades |
| **Expectancy** | Expected profit per trade | Positive |

### Expectancy Formula

```
Expectancy = (WinRate × AvgWin) - ((1-WinRate) × AvgLoss)

Example:
Win rate: 54.5%, Avg win: $7.42, Avg loss: $8.26
Expectancy: (0.545 × 7.42) - (0.455 × 8.26) = +$0.29/trade

Positive expectancy × high trade count = steady profit
```

## Phase 2: DIAGNOSE

**Key: find the root cause, not the symptom**

### Common Root Causes

| Symptom | Possible Root Cause | Diagnostic Method |
  |------|---------|---------|
| Low win rate | Strategy doesn't fit the market state | Check ADX / market regime |
| Low returns | Poor SL/TP ratio | Analyze profit/loss distribution |
| Large drawdown | Leverage too high | Compute risk exposure |
| Few trades | Conditions too strict | Loosen entry conditions |

### Market State Analysis

```
┌─────────────────────────────────────────────────────────────────┐
  │  Market State vs Strategy Fit                                  │
  │                                                                 │
  │  ADX < 25 (ranging)  → Grid Trading, Mean Reversion            │
  │  ADX > 25 (trending) → Trend Following, Momentum               │
  │  High volatility     → Scalping, reduce position size          │
  │  Low volatility      → Grid Trading, prepare for breakout      │
  │                                                                 │
  │  ⚠️ 90% of the time the market is ranging; trend strategies    │
  │     will keep losing!                                           │
  └─────────────────────────────────────────────────────────────────┘
  ```

### Diagnostic Example

```
Symptom: win rate only 25%
  ↓
Analysis: using an RSI+MACD trend strategy
↓
Check: average ADX < 25 (ranging 90% of the time)
↓
Root cause: trend strategy used in a ranging market
↓
Direction: need a ranging-market strategy (Grid Trading)
```

## Phase 3: RESEARCH

### Strategy-to-Market-State Mapping

| Market State | Recommended Strategy | Reason |
  |---------|---------|------|
| **Ranging** (ADX < 25) | Grid Trading | Exploit price oscillation |
| | Mean Reversion | Revert to the mean |
| | Scalping | Quick in and out |
| **Trending** (ADX > 25) | Trend Following | Go with the trend |
| | Momentum | Ride the momentum |
| | Breakout | Follow the breakout |
| **High volatility** | Scalping | Fast profit |
| | Reduce position | Control risk |

### Sharp Edge: Validate Research Data

```
┌─────────────────────────────────────────────────────────────────┐
  │  ⚠️ SE-1: Research data must be validated with real testing    │
  │                                                                 │
  │  Severity: critical                                             │
  │                                                                 │
  │  Case:                                                          │
  │  - Research claimed chart patterns had 80-84% success rate      │
  │  - Actual backtest: made the strategy perform worse             │
  │  - Reason: stock market data doesn't apply to crypto            │
  │                                                                 │
  │  Lesson:                                                        │
  │  ❌ Implementing as soon as you see research data               │
  │  ✅ First validate by backtesting with real data                │
  └─────────────────────────────────────────────────────────────────┘
  ```

## Phase 4: IMPLEMENT

### Parameter Tuning Strategy

| Parameter | Tuning Direction | Impact |
  |------|---------|------|
| **SL/TP ratio** | Symmetric (1:1) vs asymmetric | Win rate vs risk/reward |
| **Leverage** | 1x → 10x | Return vs risk |
| **Entry threshold** | Loose vs strict | Trade count vs quality |

### Key Findings

```
┌─────────────────────────────────────────────────────────────────┐
  │  Optimization Findings                                         │
  │                                                                 │
  │  1. Symmetric SL/TP is better                                  │
  │     - 1.8%/1.8% yields ~55% win rate                           │
  │     - Outperforms 1%/3%                                        │
  │                                                                 │
  │  2. Grid Trading in ranging markets                            │
  │     - Win rate rose from 25% to 55%                            │
  │     - Key: enable when ADX < 25                                │
  │                                                                 │
  │  3. More trades + positive expectancy                          │
  │     - Small profits accumulate into a large return             │
  │     - 191 trades × $0.29/trade = +$55                          │
  └─────────────────────────────────────────────────────────────────┘
```

## Phase 5: VALIDATE

### Backtest Requirements

| Item | Minimum | Recommended |
  |------|---------|------|
| Backtest period | 60 days | 180 days |
| Trade count | 50 trades | 100+ trades |
| Out-of-sample test | Yes | Required |
| Trading costs | Included | Include slippage |

### Validation Checklist

```markdown
  □ Did the win rate improve?
□ Did returns hit the target?
□ Is the drawdown acceptable?
□ Is expectancy positive?
□ How does it perform out of sample?
```

## Phase 6: ITERATE

```
Target not met → back to ANALYZE
  │
    ├── Analyze the new results
    ├── Adjust parameters or strategy
    ├── Re-validate
    └── Repeat until target met
```

## Best Practices

1. **Diagnose before acting** - Don't blindly tune parameters
2. **Change one variable at a time** - So you know what worked
3. **Validate research conclusions** - Theoretical data can't be trusted
4. **Enough trades** - For statistical significance
5. **Document successful experience** - Write it up as an ADR

## Common Mistakes

| Mistake | Consequence | Fix |
  |------|------|------|
| ❌ Only tuning parameters, never changing strategy | Can't solve the underlying problem | Diagnose the root cause first |
| ❌ Backtest period too short | Overfitting | 180+ days |
| ❌ Ignoring market state | Strategy-market mismatch | Use regime detection |
| ❌ Chasing high win rate | Sacrifices risk/reward | Focus on expectancy |

## Success Case

```
Before optimization:
- Return: 0.46%
  - Win rate: 23.5%
  - Strategy: RSI+MACD (trend strategy used in a ranging market)

Diagnosis:
- ADX < 25 90% of the time (ranging)
- Trend strategy is a poor fit

After optimization:
- Return: +19.93%
  - Win rate: 54.5%
  - Strategy: Grid Trading + Regime Detection

Key changes:
1. Added a Grid Trading strategy
2. Used regime detection to switch dynamically
3. Symmetric SL/TP (1.8%/1.8%)
4. 10x leverage + 191 trades
  ```

## Risk Warning

```
⚠️ High returns come with high risk

  - The case above had a maximum drawdown of 48.84%
  - Consider lowering leverage (5x) for lower risk
  - Continuously monitor changes in market state
    - Set up a stop-loss mechanism
```

## Reference Configuration

```python
# Validated configuration
config = {
  "leverage": 10.0,          # or 5.0 to reduce risk
    "stop_loss_pct": 0.018,    # 1.8%
    "take_profit_pct": 0.018,  # 1.8%
    "top_n_signals": 3,
  "min_confidence": 0.5,
  "use_regime_selector": True,
}
```

## Related Skills

- `finance/quant-trading` - Quantitative trading fundamentals
- `finance/investment-analysis` - Investment analysis
- `methodology/knowledge-acquisition-4c` - Learning a new domain
