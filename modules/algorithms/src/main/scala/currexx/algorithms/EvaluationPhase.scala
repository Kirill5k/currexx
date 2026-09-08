package currexx.algorithms

/** Which part of a run an evaluation belongs to, for an objective whose score depends on how far the search has got.
  *
  * A GA scores a population three kinds of times: once on the initial draw, once per generation, and once more on the finished population
  * before validation. Only the caller knows which of those it is asking for, and an interpreter that counts evaluation batches to work it
  * out is coupled to the exact shape of `Algorithm.GA` - insert one scoring anywhere and every phase after it is mislabelled, silently.
  *
  * Most objectives ignore this, being pure functions of the individual. `Evaluator.cached` is the case that has to: it caches by
  * individual, which is sound precisely because the objective it wraps is never handed a phase and so cannot vary with one.
  */
enum EvaluationPhase:
  /** One of the search's own scorings, numbered from zero for the initial population.
    *
    * An objective is free to read a different slice of its evidence per generation - withholding a fold in rotation, say, so that
    * selection cannot climb one regime - which is what makes these figures incomparable between generations and across runs.
    */
  case Search(generation: Int)

  /** The finished population, scored on everything the objective has.
    *
    * Run once, after selection has stopped and before validation, so that the training figure a run reports means the same as another
    * run's however either of them sliced its evidence along the way.
    */
  case Rescore
