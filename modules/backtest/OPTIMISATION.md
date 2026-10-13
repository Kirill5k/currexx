# Configure optimisation

Set the search target on each `OptimisationRound`. `Optimiser` uses the same
simulation, scoring and selection flow for all targets.

| Target | Indicator parameters | Rule numbers and durations |
|---|---|---|
| `OptimisationTarget.Indicator` | Can change | Fixed |
| `OptimisationTarget.Strategy` | Fixed | Can change within bounds |
| `OptimisationTarget.Both` | Can change | Can change within bounds |

The default is `Both`. GA and SCGA support all three targets. `Both` uses
one population and one evaluation budget. Each member is a complete
`TestStrategy`. If no rule parameters can change, `Both` keeps the rules fixed
and searches indicator parameters. Its target remains `Both`.

Rule search preserves actions, rule order, condition structure and categorical
values. Directions, zones, regimes, positions, roles and comparison operators
stay fixed. To compare these choices, define separate named base strategies and
optimise each in its own round.

## Select a round

`currexx.backtest.optimizer.OptimisationRounds.rounds` contains the scheduled
batch. Each family has GA refine and explore rounds. Selected families also have
an SCGA explore round. These rounds use the default target, `Both`.

Use `roundsByName` or `findByName` to select a scheduled round. For example,
`s10_optimized_ga_refine` uses GA and `s10_v2_scga_explore` uses SCGA. To search
only rule parameters, set `rounds` in `Optimiser` to:

```scala
val rounds = List(
  OptimisationRounds.roundsByName("s10_optimized_ga_refine").copy(target = OptimisationTarget.Strategy)
)
```

Import `OptimisationTarget` from `currexx.backtest.optimizer`. Use
`target = OptimisationTarget.Indicator` to search only indicator parameters, or
`OptimisationRounds.rounds` to run the scheduled batch. The bounds are research
settings, not validated financial recommendations.

```sh
sbt "backtest/runMain currexx.backtest.Optimiser"
```

For walk-forward evaluation, set `roundName` in `WalkForwardEvaluator`. Each
window keeps the target and creates fresh search state. See
[Walk-forward evaluation](WALK_FORWARD.md).

## Default rule bounds

Each round has a concrete bounds value:

```scala
ruleBounds: RuleParameterBounds = RuleParameterBounds.default
```

The shared preset supplies rule bounds for both joint and rule-only search.
For example, select rule-only search with:

```scala
val strategyRound = round.copy(target = OptimisationTarget.Strategy)
```

Indicator mode ignores rule bounds because its rules stay fixed.
`RuleParameterBounds.default` defines these research ranges:

| Target | Matching parameter | Minimum | Maximum | Step |
|---|---|---|---|---|
| `RuleTarget.AtrStop` | `PriceMovedAgainstEntry` ATR multiplier | `0.5` | `20.0` | `0.5` |
| `RuleTarget.TrendAge` | `TrendActiveFor` duration | `0 hours` | `2 days` | `1 hour` |
| `RuleTarget.HoldingTime` | `PositionOpenFor` duration | `0 hours` | `2 days` | `1 hour` |

Each named range applies to all matching conditions in the rule tree, including
leaves inside `Not`. A named kind that is absent from the strategy is ignored.
This lets the same preset serve different rule trees. Other parameters stay
fixed. `ValueIs` levels and velocity thresholds need explicit `At` ranges because
their units depend on the strategy.

A named target groups matching occurrences by their base value. Equal base
values form one linked search coordinate; different base values form separate
coordinates. Thus equal long and short stops stay linked, while unequal stops
remain independent. These groups stay fixed for the run. All leaves in a group
receive the same value during initialisation, mutation and crossover. An active
seed must also use one value throughout each group.

Defaults do not expand or change to fit the base. A base outside a range or
between steps fails validation before simulation. Supply explicit bounds or
adjust the base to resolve the error.
Strategy mode requires at least one active rule coordinate. To use Strategy mode
with a base that has no supported default kind, supply explicit bounds for an
available numeric or duration parameter. Both mode permits no active rule
coordinates and searches indicator parameters with the base rules fixed.

These defaults define a starting research search space. They are not validated
financial recommendations.

## Override rule bounds

Set `ruleBounds` to a custom `RuleParameterBounds` to replace the preset. It is
the complete rule search configuration; it does not merge with the preset.
Omitted numeric and duration parameters stay fixed. For example, this searches
only ATR stops:

```scala
import currexx.backtest.optimizer.*
import currexx.backtest.optimizer.strategy.*

val customRound = OptimisationRounds.roundsByName("s10_optimized_ga_refine").copy(
  name = "s10_joint_trial",
  target = OptimisationTarget.Both,
  ruleBounds = RuleParameterBounds(
    numeric = Map(RuleTarget.AtrStop -> RuleParameterBounds.DoubleRange(2.0, 6.0, 0.25))
  )
)
```

Supply a minimum, maximum and positive step. The base value must lie on that
exact grid. A range with equal endpoints fixes the value. `RuleParameterBounds()`
fixes every rule parameter. Empty bounds or a configuration with only fixed ranges
fail the active-coordinate check in Strategy mode. Both mode accepts these
configurations and searches indicator parameters with the base rules fixed.

Use `numeric` for `AtrStop` and `durations` for `TrendAge` and `HoldingTime`.
`RuleTarget.At(path, template)` selects one numeric or duration field in the
matching map. Its `template` is the complete base `TradeStrategy` snapshot.

Use `At` when one occurrence must change independently. Paths use zero-based
indices: select `Open` or `Close`, the rule index, and condition child indices.
Use child index `0` inside `Not`. For a `ValueIs` at open rule `0`:

```scala
val valueBounds = RuleParameterBounds(
  numeric = Map(
    RuleTarget.At(RuleParameterPath(RulePath(RuleList.Open, 0), RuleParameter.Level), base.rules) ->
      RuleParameterBounds.DoubleRange(20.0, 80.0, 1.0)
  )
)
```

Pass this configuration as `ruleBounds = valueBounds` for that base. This
also fixes parameters omitted from `valueBounds`, including any default groups.
The level range uses the units of the fixed role. Use `DurationRange` in the
`durations` map for time parameters. Nonnegative durations and absolute velocity
thresholds, and positive ATR multipliers, are required even when fixed.

In Strategy and Both modes, an `At` path must identify a supported parameter.
Its template must equal the current base rules. A changed or reordered rule tree
fails this check before the path is resolved. Update the snapshot and check the
path when you change the base rules. Reports store the snapshot with the path.
A missing or invalid path fails validation. Every target must use the correct
bounds map, even if its named kind is absent. Two targets must not select the
same leaf. For example, do not combine `AtrStop` with an `At` target for one of
its ATR leaves. Bounds errors identify the target or parameter. Invalid base
settings stop the run before simulation.

Strategy and Both modes check required indicator inputs before simulation,
including conditions inside `Not`.
`PriceCrossedLine` requires a crossing detector for its fixed role; a value
tracker alone does not supply this input. The base rules determine which value
trackers are active in Indicator and Both modes. This set stays constant for
the run. Explicitly fixed indicators and the raw-close price tracker stay fixed.
Use `fixedIndicators` only in Indicator or Both mode. Strategy mode fixes every
indicator to the base and rejects a nonempty `fixedIndicators` setting.

## Initial population and distance

The starting population includes the base, accepted seeds, clones, nearby
candidates and independent draws. Nearby numbers use a Gaussian step with a
standard deviation of 10% of their range, multiplied by the nearby radius.
The radii are `1`, `3` and `6` in explore mode, and `1` in refine mode. Generated
values are snapped to the declared step and kept within bounds.

Rule distance uses normalized numeric and duration differences. Each active
named group or `At` target counts once. Fixed parameters do not count. Joint
distance weights indicator and rule distances by their active coordinate counts.
For a Strategy round with only `AtrStop` active, the SCGA radius of `0.15`
applies to the normalized ATR range.

## Seeds and results

Use `NamedStrategy(name, strategy)` for extra seeds. Indicator mode takes seed
indicators with base rules. Strategy mode takes seed rules with base indicators.
Both mode uses the complete seed when its rules are compatible. If its rule
actions, structure or categorical values differ, Both mode can use its compatible
indicators with the base rules. The report states when it restores the base rules.
Strategy mode excludes incompatible rule schemas. Fixed numeric values are
restored before duplicate checks. Invalid active numbers or durations, including
off-grid values and conflicting values in a linked group, exclude the seed in
both modes. These errors do not cause the base-rule fallback. Reports state the
reason for each exclusion.

The base and accepted distinct seeds count toward the shortlist limit. Increase
`shortlistSize` if these protected candidates exceed it. The requested initial
population must also hold the base and all distinct accepted seeds. Every target
rejects a smaller initial population before generating candidates. The seed label
`fixed inputs restored` refers to changes within the active component; replacing
the inactive component with the base does not set this label.

Progress and finalist text show the searched component. The report records the
complete base as JSON on one `Base candidate` line, plus configured rule bounds
and parameter changes. One `Recorded decision` JSON line preserves the complete
approved candidate, rejected candidates and their selection evidence. Configured
bounds include unused named ranges; the target and base identify which ranges
apply. Indicator mode records the configuration but ignores it during search.
Financial records retain coverage, initial balances, monthly profits and
comparison metrics under matching human labels. JSON evidence uses format version
`3` and complete candidate identities. In Indicator mode,
catalogue entries with different rules are indicator-only matches. In Strategy
mode, entries with different indicators are rule-only matches. Both mode also
labels a catalogue entry as indicator-only when it uses the base-rule fallback.
Component matches do not identify the same complete strategy.

Selection and caches use complete candidates. Forward evaluation freezes the
approved indicators and rules together. If no candidate passes the upgrade
policy, it retains the complete base. Search results do not promote a strategy
or change live trading settings.
