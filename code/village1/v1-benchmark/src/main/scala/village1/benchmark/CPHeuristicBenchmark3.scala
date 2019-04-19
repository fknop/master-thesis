package village1.benchmark

import oscar.algo.search.Branching
import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.search.cp.heuristic.MostAvailableHeuristicDynamic
import village1.util.Utils


object CPHeuristicBenchmark3 extends CommandLineBenchmark with Branchings {

  val name = s"cp-heuristic-cos${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))
  val names = Array("MD", "COS", "MS")

  val solvers = Array(
    (b: BenchmarkRunner) => solveCP(b),
    (b: BenchmarkRunner) =>  solveCP(b, applyToSearch = search => {
      class H extends MostAvailableHeuristicDynamic(search, search.flatWorkers, search.workerVariables) {
        override def branching: Branching = conflictOrderingSearch(x, varHeuristic, valueHeuristic)
      }

      search.heuristic = new H()
    }),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => {
      class H extends MostAvailableHeuristicDynamic(search, search.flatWorkers, search.workerVariables) {
        override def branching: Branching = binaryIdx(x, i => x(i).max, valueHeuristic)
      }

      search.heuristic = new H()
    })
  )

  BenchmarkRunner.run(name, options, names, solvers)
}
