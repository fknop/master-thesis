package village1.benchmark

import BenchmarkSolverFunctions._
import village1.json.JsonSerializer

object CPRelaxationsBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = "data/benchmark/cp-relaxations.json"))

  val benchmark = new SolverBenchmark(options = options)
  val random = benchmark.run(solveCP(benchmark))
  val prop = benchmark.run(solveCPPropagationRelax(benchmark))

  val instance = benchmark.makeInstance(("CP-Random", random), ("CP-Prop", prop))

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
