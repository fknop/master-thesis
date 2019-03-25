package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}
import village1.util.Utils

object CP_MIP_CPMIP_Benchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-mip${Utils.randomInt(0, 100)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP", solveCP(benchmark))
  val (t1, o1) = benchmark.run("MIP", solveMIP(benchmark))
  val (t2, o2) = benchmark.run("CP+MIP", solveCP_MIP(benchmark))

  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1, t2), objectiveSeries = Seq(o0, o1, o2, benchmark.lowerBoundSerie()))

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}
