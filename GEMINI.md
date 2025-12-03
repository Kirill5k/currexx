# Currexx Project Context

## Project Overview
**Currexx** is an event-driven platform for automated trading, built on a functional and modular architecture using Scala 3. It is designed for currency exchange analysis, trading signal generation, backtesting, and genetic algorithm-based strategy optimization.

## Module Architecture
The project is organized as a multi-module sbt project with clear separation of concerns:

- **`domain`**: Defines the core business data types and entities. Pure domain definitions; contains **no** database or infrastructure code.
- **`algorithms`**: Contains the core definition of the Genetic Algorithm (GA) used for optimization. Implemented using the **Free Monad** pattern with the `cats-free` library.
- **`calculations`**: Implements financial indicators and statistical calculations using **pure Scala functions**.
- **`core`**: The heart of the application. Contains the **core business logic**, application services, and the HTTP server. Handles authentication, configuration, and orchestrates the trading pipeline.
- **`backtest`**: Infrastructure for running historical simulations of trading strategies. Also contains the implementation for **optimizing** trading strategies using the genetic algorithm defined in `algorithms`.
- **`clients`**: HTTP clients for external market data providers (Oanda, XTB, AlphaVantage, TwelveData) using `sttp`.

## Core Application Pipeline
The application's core logic follows a sequential, event-driven pipeline that transforms raw market data into trading actions:

1.  **Scheduled Monitoring**: A `Monitor` is defined for a set of currency pairs with a specific Schedule (e.g., cron or interval). The `MonitorService` triggers the pipeline when a monitor is due.
2.  **Data Fetching**: A `FetchMarketData` action is dispatched. A listener fetches the latest OHLCV time-series data from external APIs, decoupling scheduling from execution.
3.  **Signal Detection**: The `SignalDetector` engine applies user-defined `Indicator` calculations (e.g., moving averages, oscillators) to the fresh data. If a `Condition` is met, a `Signal` event is emitted.
4.  **Semantic State Management**: The `Signal` updates the **Market State** for the currency pair. This tracks high-level concepts (e.g., `trendIsUpward`, `momentumIsInOversold`) rather than raw values.
5.  **Rule-Based Strategy Evaluation**: The user's `TradeStrategy` (composed of semantic `openRules` and `closeRules`) is evaluated against the current Market State.
6.  **Trade Execution**: If rules are met, a `TradeAction` (OpenLong, OpenShort, ClosePosition) is dispatched to the brokerage API execution component.

## Optimization Engine
Before live deployment, strategies are improved using the offline **Optimizer** located in the `backtest` module:
- Uses a **Genetic Algorithm (GA)** to discover optimal parameters.
- Creates a "population" of parameter sets and backtests them against historical data.
- "Breeds" (crossover) and "mutates" the best-performing sets over hundreds of generations to evolve profitable strategies.

## Indicators & Data Transformations

### Indicator Types (`Indicator`)
Defines high-level analytical goals. (Defined in `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala`)
- **Composite**: Combines indicators (All/Any logic).
- **TrendChangeDetection**: Detects turning points.
- **ThresholdCrossing** / **LinesCrossing**: Value vs. Boundary or Line vs. Line.
- **KeltnerChannel** / **BollingerBands**: Price vs. Band crossings.
- **VolatilityRegimeDetection**: High vs. Low volatility states.
- **ValueTracking**: Tracks specific values for rules.
- **PriceLineCrossing**: Price vs. Calculated Line.

### Data Transformations (`ValueTransformation`)
Mathematical functions applied to price data: (Defined in `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala`)
- **Moving Averages**: SMA, EMA, WMA, HMA, NMA (Nyquist), JMA (Jurik).
- **Momentum Oscillators**: STOCH (Stochastic), RSX, JRSX (Jurik RSI).
- **Filters**: Kalman (smoothing), KalmanVelocity (momentum).
- **Statistical**: StandardDeviation.
- **Sequencing**: Chainable transformations.

### Price Sources
- Close, Open, HL2, HLC3.

## Strategy Rule Conditions
Strategies are built using readable, semantic conditions. The overall `TradeStrategy` is defined in `modules/core/src/main/scala/currexx/core/trade/TradeStrategy.scala`.

- **Logical**: `AllOf`, `AnyOf`, `Not`.
- **Position State**: `NoPosition`, `PositionIs`, `PositionOpenFor`.
- **Trend State**: `TrendIs`, `TrendChangedTo`, `TrendActiveFor`.
- **Crossover & Bands**: `CrossoverOccurred`, `UpperBandCrossed`, `LowerBandCrossed`, `PriceCrossedLine`.
- **Momentum State**: `MomentumIs` (direction), `MomentumIsIn` (zone), `MomentumEntered`, `MomentumValueIs`.
- **Volatility State**: `VolatilityIs` (High/Low).
- **Velocity State** (Kalman): `VelocityIs`, `VelocityIsBelow`, `VelocityCrossedLevel`.

## Key Technologies
- **Language**: Scala 3.7.2
- **Effect System**: Cats Effect 3
- **Functional Abstractions**: Cats Free (Algorithms), Monads.
- **Streams**: FS2 (used in data pipelines/core).
- **HTTP**: Tapir / Http4s / Sttp 4.
- **Database**: MongoDB (via `mongo4cats`).
- **JSON**: Circe.
- **Configuration**: PureConfig.
- **Math**: Breeze (Calculations).

## Development
- **Build System**: sbt
- **Commands**:
    - `sbt compile`
    - `sbt "testOnly *YourTestClassName*"`
    - `sbt test`
    - `sbt "project core" run`
    - `sbt docker:publishLocal`
- **Backtesting**: 
  - Run the `Backtest` in the `backtest` module with configured strategies and historical data.
  - `sbt "backtest/runMain currexx.backtest.Backtester"`