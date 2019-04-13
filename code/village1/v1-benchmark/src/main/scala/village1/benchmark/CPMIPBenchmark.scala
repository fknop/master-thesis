package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, BenchmarkRunner}
import village1.util.Utils

object CPMIPBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-mip${Utils.randomInt(0, 100)}.json"))

  val benchmark = new BenchmarkRunner(options = options)
  val (t0, o0, oot0) = benchmark.run("CP", solveCP(benchmark))
  val (t1, o1, oot1) = benchmark.run("MIP", solveMIP(benchmark))

  val lb = benchmark.lowerBoundSerie()
  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))


  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}
