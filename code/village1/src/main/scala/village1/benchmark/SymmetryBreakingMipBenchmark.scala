package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._

object SymmetryBreakingMipBenchmark extends App {

  val benchmark = new SolverBenchmark(Repeat = 1, SolutionLimit = 1, TimeLimit = 30)
  val cpResultsSB = benchmark.run(solveMIP(benchmark))
  val cpResultsNoSB = benchmark.run(solveMIPWithSymmetries(benchmark))

//  val tdataset = BenchmarkChart.createRuntimeDataset(("CP", cpResultsNoSB(0)), ("CP-SB", cpResultsSB(0)))
//  val tplot = BenchmarkChart.createRuntimePlot(tdataset)
//
//  val odataset = BenchmarkChart.createObjectiveDataset(("CP", cpResultsNoSB(0)), ("CP-SB", cpResultsSB(0)))
//  val oplot = BenchmarkChart.createObjectivePlot(odataset)
//
//  val plot = BenchmarkChart.combinePlot(tplot, oplot)
//
//  BenchmarkChart.show(plot, title = s"Benchmark: T=${benchmark.T(0)}")
}
