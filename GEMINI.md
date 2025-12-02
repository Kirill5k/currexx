# Currexx Project Context

## Project Overview
**Currexx** is a modular Scala 3 application designed for currency exchange analysis, trading signal generation, and backtesting. It leverages a functional programming stack based on the Typelevel ecosystem (Cats, FS2, Http4s).

### Architecture
The project is organized as a multi-module sbt project:

- **`domain`**: Core business entities and data models.
- **`algorithms`**: Optimisation algorithms and mathematical models built using `cats-free`.
- **`calculations`**: Financial indicators and statistical calculations.
- **`clients`**: HTTP clients for external market data providers (Oanda, XTB, AlphaVantage, TwelveData) using `sttp`.
- **`core`**: The main application server exposing REST endpoints via `tapir` and `http4s`. Handles authentication and config.
- **`backtest`**: Infrastructure for running historical simulations of trading strategies.

## Key Technologies
- **Language**: Scala 3.7.2
- **Effect System**: Cats Effect 3 (implied via dependencies)
- **Streams**: FS2
- **HTTP Server**: Tapir / Http4s
- **HTTP Client**: Sttp 4
- **Database**: MongoDB (via `mongo4cats`)
- **JSON**: Circe
- **Configuration**: PureConfig
- **Math**: Breeze

## Development Environment

### Prerequisites
- JVM (Java 25 recommended, as per `build.sbt` Docker base)
- sbt
- Docker (for running MongoDB and packaging)

### Build & Run
The project uses `sbt` for all build tasks.

| Task | Command |
|------|---------|
| **Compile** | `sbt compile` |
| **Test** | `sbt test` |
| **Run Core** | `sbt "project core" run` |
| **Docker Build** | `sbt docker:publishLocal` |

### Configuration
Configuration is managed via `pureconfig` and stored in `modules/core/src/main/resources/application.conf`.
Key environment variables include:
- `MONGO_USER`, `MONGO_PASSWORD`, `MONGO_HOST`: Database credentials.
- `JWT_SECRET_KEY`: For authentication.
- `ALPHA_VANTAGE_API_KEY`, `TWELVE_DATA_API_KEY`: External API keys.

### Deployment
- **Docker**: The `core` module produces a Docker image (`kirill1/currexx-core`) based on `amazoncorretto:25-alpine`.
- **Kubernetes**: Deployment manifests are located in `deployment/core.yaml`, defining a Service and Deployment for the core application.

## Directory Structure
- `modules/`: Source code for all sub-modules.
- `project/`: sbt build configuration and dependencies.
- `deployment/`: Kubernetes manifests.
- `build.sbt`: Main build definition.

## Conventions
- **Functional Programming**: The codebase heavily relies on functional idioms (Monads, Tagless Final or similar, Streams).
- **Type Safety**: Usage of Refined types and tagged ADTs for domain modeling.
- **Testing**: Integration tests likely use embedded Mongo and HTTP stubs.
