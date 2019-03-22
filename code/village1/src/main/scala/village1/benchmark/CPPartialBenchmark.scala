package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.json.JsonSerializer
import village1.modeling.cp.CPModelOptions

object CPPartialBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = "data/benchmark/cp-partial.json"))

  val benchmark = new SolverBenchmark(options = options)
  val random = benchmark.run("CP-Partial", solveCP(benchmark))
  val prop = benchmark.run("CP-Full", solveCP(benchmark, CPModelOptions().copy(allowPartial = false)))

  val instance = benchmark.makeInstance(random, prop)

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
