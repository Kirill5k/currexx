# Strategy Optimization Scoring Functions Guide

## Quick Reference

| Your Goal | Function | Configuration |
|-----------|----------|---------------|
| **Best starting point** | `balanced()` | Default |
| **Maximum returns** | `balanced()` | `profitWeight=0.6-0.7` |
| **High-quality trades** | `balanced()` | `ratioWeight=0.5-0.6` |
| **Steady income** | `balanced()` | `consistencyWeight=0.5` |
| **Risk management** | `riskAdjusted()` | Default |

## Quick Start

```scala
// In Optimiser.scala - Recommended default
val scoringFunction = ScoringFunction.balanced(
  profitWeight = 0.4,          // 40% weight on total profit
  ratioWeight = 0.3,           // 30% weight on win/loss ratio
  consistencyWeight = 0.3,     // 30% weight on monthly consistency
  minOrders = Some(50),
  maxOrders = Some(700)
)
```

## Available Functions

### 1. `balanced()` - Multi-Objective (⭐ Recommended)

Combines profit, win/loss ratio, and consistency with configurable weights.

```scala
ScoringFunction.balanced(
  profitWeight = 0.4,          // Focus on absolute returns
  ratioWeight = 0.3,           // Focus on trade quality
  consistencyWeight = 0.3,     // Focus on steady monthly performance
  minOrders = Some(30),        // Min trades per dataset
  maxOrders = Some(500),       // Max trades per dataset
  targetRatio = 2.0            // Target win/loss ratio
)
```

**How it works:** Normalizes and weights each component, penalizes extreme order counts.

**Pros:** Balances multiple objectives, prevents skewed optimization, highly configurable  
**Cons:** Requires weight tuning

### 2. `riskAdjusted()` - Risk-Aware Returns

Optimizes profit per unit of risk: `totalProfit / avgBiggestLoss`

```scala
ScoringFunction.riskAdjusted(
  minOrders = Some(30),
  maxOrders = Some(500)
)
```

**Pros:** Controls drawdown, good for capital preservation  
**Cons:** May sacrifice absolute returns

### 3. Simple Functions (Single Objective)

```scala
ScoringFunction.totalProfit                           // Pure profit (ignores quality)
ScoringFunction.medianWinLossRatio(Some(50), Some(700))  // Pure quality (ignores profit)
ScoringFunction.averageMedianProfitByMonth            // Pure consistency
```

**Warning:** Single-objective functions produce skewed results. Use `balanced()` instead.

## Configuration Examples

### Aggressive (Max Profit)
```scala
ScoringFunction.balanced(
  profitWeight = 0.6, ratioWeight = 0.2, consistencyWeight = 0.2,
  maxOrders = Some(1000), targetRatio = 1.5
)
```

### Conservative (High Quality)
```scala
ScoringFunction.balanced(
  profitWeight = 0.3, ratioWeight = 0.5, consistencyWeight = 0.2,
  maxOrders = Some(300), targetRatio = 2.5
)
```

### Income Generator (Steady Returns)
```scala
ScoringFunction.balanced(
  profitWeight = 0.3, ratioWeight = 0.2, consistencyWeight = 0.5,
  minOrders = Some(40)
)
```

### Risk Manager
```scala
ScoringFunction.riskAdjusted(minOrders = Some(30), maxOrders = Some(500))
```

## Understanding Weights

| Weight | Higher (0.5-0.7) | Lower (0.2-0.3) | Effect |
|--------|------------------|-----------------|--------|
| **profitWeight** | Max returns | Quality/consistency focus | Controls profit priority |
| **ratioWeight** | High-quality trades only | Allows lower quality if profitable | Controls trade quality threshold |
| **consistencyWeight** | Steady monthly returns | Allows volatile performance | Reduces monthly variance |

**Note:** Weights should sum to ~1.0

## Tuning Process

1. **Start with defaults:** `ScoringFunction.balanced()`
2. **Run optimization** and observe: total profit, win/loss ratio, monthly variance, order count
3. **Adjust weights** based on results (use 0.1 increments):

| Problem | Solution |
|---------|----------|
| Profit too low | Increase `profitWeight` to 0.5-0.6 |
| Too many losses | Increase `ratioWeight` to 0.4-0.5 |
| Inconsistent monthly | Increase `consistencyWeight` to 0.4 |
| Too many trades | Lower `maxOrders` to 400-500 |
| Too few trades | Lower `minOrders` to 20-30 |
| High drawdown | Switch to `riskAdjusted()` |

4. **Validate** on out-of-sample data

## Custom Scoring Functions

```scala
val custom: ScoringFunction = stats => {
  if (stats.isEmpty) BigDecimal(0)
  else {
    val totalProfit = stats.foldLeft(BigDecimal(0))(_ + _.totalProfit)
    val avgRatio = stats.map(_.winLossRatio).sum / BigDecimal(stats.size)
    val avgOrders = stats.map(_.total).sum / stats.size
    
    // Example: profit * sqrt(ratio) with order count penalty
    val baseScore = totalProfit * BigDecimal(Math.sqrt(avgRatio.toDouble))
    val penalty = if (avgOrders < 30 || avgOrders > 700) 0.5 else 1.0
    (baseScore * BigDecimal(penalty)).roundTo(5)
  }
}
```

### Available OrderStats Fields

```scala
case class OrderStats(
  total: Int,                           // Total trades
  buys: Int, sells: Int,                // Trade counts
  totalProfit: BigDecimal,              // Sum of all P/L
  biggestWin: BigDecimal,               // Largest win
  biggestLoss: BigDecimal,              // Largest loss (negative)
  losses: List[BigDecimal],             // Individual losses
  profitByMonth: Map[String, BigDecimal] // Monthly breakdown
)
// Computed:
def winLossRatio: BigDecimal           // (wins / losses)
def medianProfitByMonth: BigDecimal    // Median monthly profit
def meanProfitByMonth: BigDecimal      // Average monthly profit
def meanLoss: BigDecimal               // Average loss
```

## Best Practices

✅ **Do:**
- Start with `balanced()` defaults, then iterate
- Keep weights summing to ~1.0
- Use 30-500 order count range
- Test on multiple datasets (currency pairs)
- Validate on out-of-sample data
- Run A/B tests with different configs

❌ **Don't:**
- Use single-objective functions (causes skewed results)
- Set extreme constraints (min=10, max=2000)
- Optimize on single dataset
- Trust results without validation
- Change multiple weights simultaneously

## Pro Tips

1. **A/B Testing:** Run optimization 2-3 times with different scoring functions, compare results
2. **Incremental tuning:** Adjust one weight at a time by ±0.1
3. **Market-specific:** Different markets may need different weights
4. **Hybrid approach:** Use `balanced()` + manual review of OrderStats
5. **Conservative start:** Begin with defaults, gradually move toward your goals

## Technical Details

**Type signature:**
```scala
type ScoringFunction = List[OrderStats] => BigDecimal
```

Each `OrderStats` represents one test dataset (e.g., one currency pair). The function aggregates across all datasets to produce a single score for the genetic algorithm.

**Implementation:** See `modules/backtest/src/main/scala/currexx/backtest/optimizer/IndicatorEvaluator.scala`

---

**Remember:** The best scoring function depends on your goals, risk tolerance, and market conditions. Start with `balanced()` and iterate! 🎯
