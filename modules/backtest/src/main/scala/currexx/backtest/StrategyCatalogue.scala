package currexx.backtest

/** Named strategy definitions, including lineage entries omitted from batch evaluation. */
object StrategyCatalogue {
  final case class Entry(name: String, strategy: TestStrategy, includeInBatch: Boolean)

  val entries: List[Entry] = List(
    Entry("s2_optimized", TestStrategy.s2_optimized, includeInBatch = true),
    Entry("s2_optimized_v2", TestStrategy.s2_optimized_v2, includeInBatch = true),
    Entry("s10", TestStrategy.s10, includeInBatch = true),
    Entry("s10_optimized", TestStrategy.s10_optimized, includeInBatch = true),
    Entry("s10_v2", TestStrategy.s10_v2, includeInBatch = true),
    Entry("s5_optimized_v2", TestStrategy.s5_optimized_v2, includeInBatch = true),
    Entry("s5_optimized_v3", TestStrategy.s5_optimized_v3, includeInBatch = true),
    Entry("s1_v2_optimized", TestStrategy.s1_v2_optimized, includeInBatch = true),
    Entry("s1_v2_optimized_v4", TestStrategy.s1_v2_optimized_v4, includeInBatch = true),
    Entry("s4_optimized_v1", TestStrategy.s4_optimized_v1, includeInBatch = true),
    Entry("s4_optimized_v2", TestStrategy.s4_optimized_v2, includeInBatch = true),
    Entry("s6_optimized", TestStrategy.s6_optimized, includeInBatch = true),
    Entry("s6", TestStrategy.s6, includeInBatch = true),
    Entry("s13", TestStrategy.s13, includeInBatch = true),
    Entry("s13_optimized", TestStrategy.s13_optimized, includeInBatch = true),
    Entry("s12", TestStrategy.s12, includeInBatch = false),
    Entry("s12_optimized", TestStrategy.s12_optimized, includeInBatch = false)
  )

  val batch: List[(String, TestStrategy)] = entries.collect { case Entry(name, strategy, true) =>
    name -> strategy
  }
}
