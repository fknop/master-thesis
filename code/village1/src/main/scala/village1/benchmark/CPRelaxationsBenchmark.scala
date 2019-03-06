package village1.benchmark

import oscar.cp.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.benchmark.BenchmarkSolverFunctions._

object CPRelaxationsBenchmark extends App {

  val benchmark = new SolverBenchmark(Repeat = 1, DryRun = 0, SolutionLimit = Int.MaxValue, TimeLimit = 5)
  val cpResultsRandom = benchmark.run(solveCP(benchmark))
  val cpResultsProp = benchmark.run(solveCP(benchmark))
//
//  val tdataset = BenchmarkChart.createRuntimeDataset(("CP-Rand", cpResultsRandom(0)), ("CP-Prop", cpResultsProp(0)))
//  val tplot = BenchmarkChart.createRuntimePlot(tdataset)
//
//  val odataset = BenchmarkChart.createObjectiveDataset(("CP-Rand", cpResultsRandom(0)), ("CP-Prop", cpResultsProp(0)))
//  val oplot = BenchmarkChart.createObjectivePlot(odataset)
//
//  val plot = BenchmarkChart.combinePlot(tplot, oplot)
//
//  BenchmarkChart.show(plot, title = s"Benchmark: T=${cpResultsRandom(0).T}")
//
//
//  def randomRelax: (CPSolver, Array[CPIntVar], CPIntSol) => Unit = {
//    (solver: CPSolver, vars: Array[CPIntVar], sol: CPIntSol) => RelaxationFunctions.randomRelax(solver, vars, sol, vars.length / 2)
//  }
//
//  def propagation: (CPSolver, Array[CPIntVar], CPIntSol) => Unit = {
//    (solver: CPSolver, vars: Array[CPIntVar], sol: CPIntSol) => RelaxationFunctions.propagationGuidedRelax(solver, vars, sol,100)
//  }
}
