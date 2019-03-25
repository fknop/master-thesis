package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}
import village1.util.Utils

object MIPStartBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-mip${Utils.randomInt(0, 100)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val cp_mip0 = benchmark.run("CP+MIP 0.2", solveCP_MIP(benchmark, 0.2))
  val cp_mip1 = benchmark.run("CP+MIP 0.3", solveCP_MIP(benchmark, 0.3))
  val cp_mip2 = benchmark.run("CP+MIP 0.5", solveCP_MIP(benchmark, 0.5))
  val cp_mip3 = benchmark.run("CP+MIP 0.7", solveCP_MIP(benchmark, 0.7))
  val cp_mip4 = benchmark.run("CP+MIP 1", solveCP_MIP(benchmark, 1))

//  val instance = benchmark.makeInstance(cp_mip0, cp_mip1, cp_mip2, cp_mip3, cp_mip4)

//  val writer = JsonSerializer.serialize(instance)
//  writer(options.out)
}
