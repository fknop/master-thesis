package village1.benchmark

import BenchmarkSolverFunctions._
import village1.json.JsonSerializer

object SymmetryBreakingBenchmark extends App {

  val benchmark = new SolverBenchmark(Repeat = 1, SolutionLimit = Int.MaxValue, TimeLimit = 2)
  val cpResults = benchmark.run(solveCP(benchmark))
  val cpResultsSB = benchmark.run(solveCPWithSymmetries(benchmark))

  val instance = benchmark.makeInstance(("CP-NoSym", cpResults), ("CP-Sym", cpResultsSB))

  val writer = JsonSerializer.serialize(instance)
  writer("data/benchmark/test.json")

//  val tdataset = BenchmarkChart.createRuntimeDataset(("CP", cpResultsSB(0)), ("CP-SB", cpResults(0)))
//  val tplot = BenchmarkChart.createRuntimePlot(tdataset)
//
//  val odataset = BenchmarkChart.createObjectiveDataset(("CP", cpResultsSB(0)), ("CP-SB", cpResults(0)))
//  val oplot = BenchmarkChart.createObjectivePlot(odataset)

//  val plot = BenchmarkChart.combinePlot(tplot, oplot)

//  BenchmarkChart.show(plot, title = s"Benchmark: T=${cpResults(0).T}")
}
