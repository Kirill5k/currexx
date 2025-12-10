# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Currexx** is an event-driven automated trading platform built with Scala 3. It analyzes currency markets, generates trading signals using technical indicators, executes trades, and optimizes strategies through genetic algorithms and backtesting.

## Build Commands

### Essential Commands
- `sbt compile` - Compile all modules
- `sbt test` - Run all tests
- `sbt "testOnly *ClassName*"` - Run specific test class
- `sbt "project core" run` - Run the core application
- `sbt "project backtest" run` - Run backtesting
- `sbt docker:publishLocal` - Build Docker image locally

### Module-Specific Commands
- `sbt "project domain" test` - Test domain module
- `sbt "project algorithms" compile` - Compile algorithms module
- `sbt "project calculations" test` - Test calculations module
- `sbt "project clients" test` - Test clients module
- `sbt "project core" test` - Test core module

## Architecture Overview

### Multi-Module Structure

The project follows a modular architecture with clear separation of concerns:

1. **domain** - Pure domain types and business entities
   - No infrastructure code
   - Contains value objects, enums, and domain logic
   - Shared across all modules

2. **algorithms** - Genetic algorithm implementation
   - Uses Free Monad pattern with `cats-free`
   - Defines GA operators: initializers, crossover, mutation, selection, elitism
   - Infrastructure-agnostic algorithm definitions

3. **calculations** - Financial indicators and statistics
   - Pure Scala functions using Breeze library
   - Moving averages (SMA, EMA, WMA, HMA, JMA, NMA)
   - Momentum oscillators (RSX, JRSX, STOCH)
   - Filters (Kalman, KalmanVelocity)
   - Volatility measures (ATR, Standard Deviation)

4. **clients** - External API integrations
   - HTTP clients for market data providers (Oanda, XTB, AlphaVantage, TwelveData)
   - Uses sttp4 for HTTP communication
   - Implements broker interfaces for trade execution

5. **core** - Application heart
   - Business logic and orchestration
   - HTTP server (Tapir + Http4s)
   - Authentication and session management (JWT)
   - Database layer (MongoDB via mongo4cats)
   - Action dispatching and event processing

6. **backtest** - Historical simulation and optimization
   - Strategy backtesting against historical data
   - Genetic algorithm implementation for parameter optimization
   - Test services that simulate market conditions

### Event-Driven Pipeline

The core application follows a sequential pipeline triggered by scheduled monitors:

1. **Scheduling** - `MonitorService` triggers monitors based on Schedule (cron/interval)
2. **Data Fetching** - `Action.FetchMarketData` dispatched → clients fetch OHLCV time-series
3. **Signal Detection** - `SignalDetector` applies `Indicator` calculations to detect conditions
4. **State Management** - `Signal` events update semantic `MarketState` (trend, momentum, volatility)
5. **Strategy Evaluation** - `TradeStrategy` rules evaluated against `MarketState`
6. **Trade Execution** - `TradeAction` (OpenLong/OpenShort/ClosePosition) sent to broker

### Action System

The `Action` enum (modules/core/src/main/scala/currexx/core/common/action/Action.scala) defines all system events:
- `RescheduleAllMonitors` - Restart all monitor schedules
- `ScheduleMonitor` - Schedule a specific monitor
- `FetchMarketData` - Retrieve market data from external APIs
- `ProcessMarketData` - Process fetched data through signal detection
- `ProcessSignals` - Handle detected signals and update market state
- `ProcessMarketStateUpdate` - Evaluate trading rules against new state
- `ProcessTradeOrderPlacement` - Execute trade orders through broker

The `ActionDispatcher` queues actions and the `ActionProcessor` handles them asynchronously.

## Trading Strategy System

### Indicators (`Indicator`)

Indicators define what to detect in market data: (Defined in `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala`)
- **Composite** - Combine multiple indicators with All/Any logic
- **TrendChangeDetection** - Detect trend reversals
- **ThresholdCrossing** - Value crossing upper/lower boundaries (e.g., RSI overbought/oversold)
- **LinesCrossing** - Two moving averages crossing (e.g., fast EMA crossing slow EMA)
- **KeltnerChannel** / **BollingerBands** - Price crossing channel bands
- **VolatilityRegimeDetection** - High vs Low volatility states
- **ValueTracking** - Track specific calculated values (momentum, velocity, volatility)
- **PriceLineCrossing** - Price crossing a calculated line

### Value Transformations (`ValueTransformation`)

Mathematical functions applied to price data to calculate indicators: (Defined in `modules/domain/src/main/scala/currexx/domain/signal/Indicator.scala`)
- Moving Averages: SMA, EMA, WMA, HMA, NMA (Nyquist), JMA (Jurik)
- Oscillators: RSX, JRSX (Jurik RSI), STOCH (Stochastic)
- Filters: Kalman (smoothing), KalmanVelocity (momentum)
- Statistical: StandardDeviation, ATR

### Price Sources
- Close, Open, HL2, HLC3.

### Trading Rules

Strategies are defined in `TradeStrategy` (modules/core/src/main/scala/currexx/core/trade/TradeStrategy.scala) with semantic conditions:
- **Logical**: AllOf, AnyOf, Not
- **Position**: NoPosition, PositionIs, PositionOpenFor
- **Trend**: TrendIs, TrendChangedTo, TrendActiveFor
- **Crossover**: CrossoverOccurred, UpperBandCrossed, LowerBandCrossed, PriceCrossedLine
- **Momentum**: MomentumIs, MomentumIsIn, MomentumEntered
- **Volatility**: VolatilityIs, PreviousVolatilityIs
- **Velocity**: VelocityIs, VelocityIsBelow, VelocityCrossedLevel
- **Value**: ValueIs (compare tracked values)

## Strategy Optimization

The `backtest` module provides a genetic algorithm for discovering optimal strategy parameters:
1. **Population** - Generate random parameter sets
2. **Evaluation** - Evaluate each parameter set against historical data
3. **Selection** - Keep best-performing strategies
4. **Crossover** - Breed new strategies from successful ones
5. **Mutation** - Introduce random variations
6. **Evolution** - Repeat for hundreds of generations

Run via Optimiser: `sbt "backtest/runMain currexx.backtest.Optimiser"`

Testing a strategy against historical set of data via Backtester: `sbt "backtest/runMain currexx.backtest.Backtester"`

## Key Technologies

- **Language**: Scala 3.7.4
- **Effect System**: Cats Effect 3, FS2 streams
- **Functional**: Cats Free (Free Monad for algorithms)
- **HTTP**: Tapir (endpoint definitions) + Http4s (server) + Sttp4 (clients)
- **Database**: MongoDB (mongo4cats)
- **JSON**: Circe with tagged-adt-codec for ADT serialization
- **Configuration**: PureConfig
- **Math**: Breeze
- **Auth**: JWT (jwt-circe), BCrypt
- **Scheduling**: cron-utils

## Testing

- Tests use **common-scala** test utilities (common-http4s-test, common-sttp-test)
- MongoDB tests extend `MongoSpec` trait which provides `EmbeddedMongo`
- Test fixtures in modules/core/src/test/scala/currexx/core/fixtures/
- Example test pattern:
  ```scala
  class ServiceSpec extends MongoSpec {
    "Service" should {
      "do something" in {
        // test using IOWordSpec from common-scala
      }
    }
  }
  ```

## Code Style

- Scalafmt configured with Scala 3 dialect (maxColumn=140)
- Use `sbt scalafmtAll` to format code
- Follows functional programming principles with pure functions
- Extensive use of Scala 3 enums for domain types
- JSON codecs use circe-tagged-adt-codec for ADT serialization

## Database Schema

MongoDB collections (database name: "currexx"):
- **users** - User accounts with authentication
- **sessions** - Active user sessions
- **monitors** - Scheduled market monitoring jobs
- **signals** - Detected trading signals
- **marketState** - Current semantic market state per currency pair
- **tradeOrders** - Executed trade orders
- **settings** - Signal and trade configuration per user
- **logEvents** - Application logs

## Application Startup

Main entry point: `modules/core/src/main/scala/currexx/core/Application.scala`

Startup sequence:
1. Load configuration from PureConfig
2. Initialize MongoDB connection and HTTP backend
3. Create ActionDispatcher and dispatch `RescheduleAllMonitors`
4. Initialize clients, services (auth, signals, monitors, markets, trades, settings)
5. Create HTTP endpoints (Health, Auth, Signals, Monitors, Markets, Trades, Settings)
6. Start ActionProcessor, LogEventProcessor, and HTTP server in parallel streams

## File Naming Conventions

- Domain types: CamelCase files matching type name (User.scala, Monitor.scala)
- Database entities: *Entity.scala (UserEntity.scala)
- Database repositories: *Repository.scala (UserRepository.scala)
- HTTP controllers: *Controller.scala (AuthController.scala)
- Services: *Service.scala (MonitorService.scala)
- Test specs: *Spec.scala (MonitorServiceSpec.scala)
- Package-level definitions: lowercase.scala (json.scala, types.scala, effects.scala)
