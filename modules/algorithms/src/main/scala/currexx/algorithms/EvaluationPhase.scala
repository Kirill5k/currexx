package currexx.algorithms

/** Which part of a run an evaluation belongs to, for an objective whose score depends on how far the search has got.
  *
  * The algorithm supplies the phase explicitly. Multiple evaluation batches may belong to the same generation, so an interpreter must not
  * infer the phase by counting batches.
  *
  * Most objectives ignore this, being pure functions of the individual. `Evaluator.cached` is the case that has to: it caches by
  * individual, which is sound precisely because the objective it wraps is never handed a phase and so cannot vary with one.
  */
enum EvaluationPhase:
  /** One of the search's own scorings, numbered from zero for the initial population.
    *
    * An objective may assign different fitness to the same individual in different generations. Scores from different phases are not
    * guaranteed to be comparable.
    */
  case Search(generation: Int)

  /** The finished population, evaluated using the objective's final scoring policy.
    *
    * Run once, after selection has stopped and before validation, to rank the result independently of the final search generation's phase.
    */
  case Rescore
