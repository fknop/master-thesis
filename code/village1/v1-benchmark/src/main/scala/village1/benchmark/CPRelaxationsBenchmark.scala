package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}

object CPRelaxationsBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = "data/benchmark/cp-relaxations.json"))

  val benchmark = new SolverBenchmark(options = options)
  val random = benchmark.run("CP-Random", solveCP(benchmark))
  val prop = benchmark.run("CP-Prop", solveCPPropagationRelax(benchmark))

//  val instance = benchmark.makeInstance(random, prop)

//  val writer = JsonSerializer.serialize(instance)
//  writer(options.out)
}