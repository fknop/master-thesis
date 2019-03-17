package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.json.JsonSerializer
import village1.util.Utils

object CP_MIP_CPMIP_Benchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-mip${Utils.randomInt(0, 100)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val cp = benchmark.run("CP", solveCP(benchmark))
  val mip = benchmark.run("MIP", solveMIP(benchmark))
  val cp_mip = benchmark.run("CP+MIP", solveCP_MIP(benchmark))

  val instance = benchmark.makeInstance(cp, mip, cp_mip)

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
