package currexx.backtest

/** Named strategy definitions, including lineage entries omitted from batch evaluation.
  *
  * Keep batch membership and ordering explicit. StrategyCatalogueSpec checks completeness against every public TestStrategy definition.
  */
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
    Entry("s12", TestStrategy.s12, includeInBatch = true),
    Entry("s2_optimized_v18", TestStrategy.s2_optimized_v18, includeInBatch = false),
    Entry("s2_optimized_v16", TestStrategy.s2_optimized_v16, includeInBatch = false),
    Entry("s2_optimized_v17", TestStrategy.s2_optimized_v17, includeInBatch = false),
    Entry("s10_optimized_v11", TestStrategy.s10_optimized_v11, includeInBatch = false),
    Entry("s10_optimized_v12", TestStrategy.s10_optimized_v12, includeInBatch = false),
    Entry("s5_optimized_v14", TestStrategy.s5_optimized_v14, includeInBatch = false),
    Entry("s5_optimized_v15", TestStrategy.s5_optimized_v15, includeInBatch = false),
    Entry("s4_optimized_v9", TestStrategy.s4_optimized_v9, includeInBatch = false),
    Entry("s4_optimized_v10", TestStrategy.s4_optimized_v10, includeInBatch = false),
    Entry("s6_optimized_v14", TestStrategy.s6_optimized_v14, includeInBatch = false),
    Entry("s6_optimized_v15", TestStrategy.s6_optimized_v15, includeInBatch = false),
    Entry("s10_v2_optimized_v13", TestStrategy.s10_v2_optimized_v13, includeInBatch = false),
    Entry("s10_v2_optimized_v14", TestStrategy.s10_v2_optimized_v14, includeInBatch = false),
    Entry("s13_optimized_v13", TestStrategy.s13_optimized_v13, includeInBatch = false),
    Entry("s1_v2_optimized_v15", TestStrategy.s1_v2_optimized_v15, includeInBatch = false),
    Entry("s1_v2_optimized_v16", TestStrategy.s1_v2_optimized_v16, includeInBatch = false),
    Entry("s10_v2_optimized_v15", TestStrategy.s10_v2_optimized_v15, includeInBatch = false),
    Entry("s13_optimized_v11", TestStrategy.s13_optimized_v11, includeInBatch = false),
    Entry("s13_optimized_v14", TestStrategy.s13_optimized_v14, includeInBatch = false),
    Entry("s1_v2_optimized_v17", TestStrategy.s1_v2_optimized_v17, includeInBatch = false),
    Entry("s12_optimized", TestStrategy.s12_optimized, includeInBatch = false)
  )

  val batch: List[(String, TestStrategy)] = entries.collect { case Entry(name, strategy, true) =>
    name -> strategy
  }
}
