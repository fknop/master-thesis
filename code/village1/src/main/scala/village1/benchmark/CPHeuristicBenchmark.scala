package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.json.JsonSerializer
import village1.modeling.cp.CPModelOptions
import village1.util.Utils

object CPHeuristicBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-heuristic${Utils.randomInt(0, 1000)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val heuristic = benchmark.run("CP-New", solveCP(benchmark))
  val oldHeuristic = benchmark.run("CP-Old", solveCP(benchmark, applyToSearch = search => {
    search.heuristic {
      val h = new MostAvailableHeuristic2(search, search.flatWorkers)
      h.branching
    }
  }))

  val instance = benchmark.makeInstance(heuristic, oldHeuristic)

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
