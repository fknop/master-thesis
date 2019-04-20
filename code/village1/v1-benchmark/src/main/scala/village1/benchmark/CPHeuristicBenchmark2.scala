package village1.benchmark

import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.search.cp.heuristic.MostAvailableHeuristic
import village1.util.Utils


object CPHeuristicBenchmark2 extends CommandLineBenchmark with Branchings {

  val name = s"cp-heuristic-ma-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))
  val names = Array("CP-MA-D", "CP-MA")

  val solvers = Array(
    (b: BenchmarkRunner) => solveCP(b),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => {
      search.heuristic = new MostAvailableHeuristic(search, search.flatWorkers, search.workerVariables)
    }),
  )

  BenchmarkRunner.run(name, options, names, solvers)
}
