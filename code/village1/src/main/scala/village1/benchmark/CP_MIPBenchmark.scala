package village1.benchmark


import village1.benchmark.BenchmarkSolverFunctions._

object CP_MIPBenchmark extends App {

  val benchmark = new SolverBenchmark(Repeat = 2)
  val cpResults = benchmark.run(solveCP(benchmark))
  val mipResults = benchmark.run(solveMIP(benchmark))
//
//  val tdataset = BenchmarkChart.createRuntimeDataset(("CP", cpResults(0)), ("MIP", mipResults(0)))
//  val tplot = BenchmarkChart.createRuntimePlot(tdataset)
//
//  val odataset = BenchmarkChart.createObjectiveDataset(("CP", cpResults(0)), ("MIP", mipResults(0)))
//  val oplot = BenchmarkChart.createObjectivePlot(odataset)
//
//  val plot = BenchmarkChart.combinePlot(tplot, oplot)
//
//  BenchmarkChart.show(plot, title = s"Benchmark: T=${cpResults(0).T}")
}
