package currexx.backtest.optimizer.strategy

import cats.syntax.traverse.*
import cats.syntax.foldable.*
import currexx.backtest.optimizer.Checks
import currexx.core.trade.{Rule, TradeStrategy}

/** Rule-tree traversal and comparison preserve the stored node order. */
private[strategy] object RuleTree {
  def leaves(strategy: TradeStrategy): Vector[(RulePath, Rule.Condition)] =
    def collect(condition: Rule.Condition, path: RulePath): List[(RulePath, Rule.Condition)] = condition match
      case Rule.Condition.AllOf(children) => children.zipWithIndex.flatMap((child, index) => collect(child, path.child(index)))
      case Rule.Condition.AnyOf(children) => children.zipWithIndex.flatMap((child, index) => collect(child, path.child(index)))
      case Rule.Condition.Not(child)      => collect(child, path.child(0))
      case leaf                           => List(path -> leaf)
    def rules(values: List[Rule], list: RuleList): List[(RulePath, Rule.Condition)] =
      values.zipWithIndex.flatMap((rule, index) => collect(rule.conditions, RulePath(list, index)))
    (rules(strategy.openRules, RuleList.Open) ++ rules(strategy.closeRules, RuleList.Close)).toVector

  def mapLeaves(strategy: TradeStrategy)(
      change: (RulePath, Rule.Condition) => Either[IllegalArgumentException, Rule.Condition]
  ): Either[IllegalArgumentException, TradeStrategy] =
    def visit(condition: Rule.Condition, path: RulePath): Either[IllegalArgumentException, Rule.Condition] = condition match
      case Rule.Condition.AllOf(children) =>
        children.zipWithIndex.traverse((child, index) => visit(child, path.child(index))).map(Rule.Condition.AllOf.apply)
      case Rule.Condition.AnyOf(children) =>
        children.zipWithIndex.traverse((child, index) => visit(child, path.child(index))).map(Rule.Condition.AnyOf.apply)
      case Rule.Condition.Not(child) => visit(child, path.child(0)).map(Rule.Condition.Not.apply)
      case leaf                      => change(path, leaf)
    def rules(values: List[Rule], list: RuleList): Either[IllegalArgumentException, List[Rule]] =
      values.zipWithIndex.traverse((rule, index) => visit(rule.conditions, RulePath(list, index)).map(c => rule.copy(conditions = c)))
    for
      open  <- rules(strategy.openRules, RuleList.Open)
      close <- rules(strategy.closeRules, RuleList.Close)
    yield TradeStrategy(open, close)

  /** Restore numeric fields for comparison; full equality then protects all remaining fields, including future ones. */
  def validateStructure(template: TradeStrategy, candidate: TradeStrategy)(
      normaliseNumeric: (RulePath, Rule.Condition) => Either[IllegalArgumentException, Rule.Condition]
  ): Either[IllegalArgumentException, Unit] = {
    def children(as: List[Rule.Condition], bs: List[Rule.Condition], path: RulePath): Either[IllegalArgumentException, Unit] =
      Checks.check(as.size == bs.size, s"Different child count in the fixed rule tree at $path").flatMap { _ =>
        as.zip(bs).zipWithIndex.traverse_ { case ((a, b), index) => conditions(a, b, path.child(index)) }
      }
    def conditions(a: Rule.Condition, b: Rule.Condition, path: RulePath): Either[IllegalArgumentException, Unit] = (a, b) match
      case (Rule.Condition.AllOf(as), Rule.Condition.AllOf(bs)) => children(as, bs, path)
      case (Rule.Condition.AnyOf(as), Rule.Condition.AnyOf(bs)) => children(as, bs, path)
      case (Rule.Condition.Not(a), Rule.Condition.Not(b))       => conditions(a, b, path.child(0))
      case _                                                    =>
        normaliseNumeric(path, b).flatMap { normalised =>
          Checks.check(a == normalised, s"Different fixed condition at $path: expected $a, found $normalised")
        }
    def rules(as: List[Rule], bs: List[Rule], list: RuleList): Either[IllegalArgumentException, Unit] =
      Checks.check(as.size == bs.size, s"Different rule count in the fixed $list rule list").flatMap { _ =>
        as.zip(bs).zipWithIndex.traverse_ { case ((a, b), index) =>
          val path = RulePath(list, index)
          Checks.check(a.action == b.action, s"Different fixed action at $path").flatMap(_ => conditions(a.conditions, b.conditions, path))
        }
      }
    for
      _ <- rules(template.openRules, candidate.openRules, RuleList.Open)
      _ <- rules(template.closeRules, candidate.closeRules, RuleList.Close)
    yield ()
  }
}
