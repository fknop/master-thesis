package village1.benchmark

import oscar.algo.search.Branching
import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.search.cp.heuristic.{Heuristic, MostAvailableHeuristic}
import village1.util.Utils


object CPHeuristicBenchmark extends CommandLineBenchmark with Branchings {

  val name = s"cp-heuristic-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val names = Array("CP-MA", "CP-MV")
  val solvers = Array(
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => {
      search.heuristic = new MostAvailableHeuristic(search, search.flatWorkers, search.workerVariables)
    }),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => {
      search.heuristic = new Heuristic {
        override def branching: Branching = binaryFirstFailIdx(search.flatWorkers, i => {
          search.flatWorkers(i).max
        })
      }
    })
  )

  BenchmarkRunner.run(name, options, names, solvers)
}
