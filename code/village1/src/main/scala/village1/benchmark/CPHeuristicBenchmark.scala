package village1.benchmark

import oscar.algo.branchings.BinaryBranching
import oscar.algo.vars.IntVarLike
import village1.benchmark.BenchmarkSolverFunctions._
import village1.json.JsonSerializer
import village1.util.Utils
import oscar.cp.modeling.Branchings


object CPHeuristicBenchmark extends CommandLineBenchmark with Branchings {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-heuristic${Utils.randomInt(0, 1000)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val heuristic = benchmark.run("CP-MA", solveCP(benchmark))
  val oldHeuristic = benchmark.run("CP-FF", solveCP(benchmark, applyToSearch = search => {
    search.heuristic {
      binaryFirstFail(search.flatWorkers)
    }
  }))

  val instance = benchmark.makeInstance(heuristic, oldHeuristic)

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
