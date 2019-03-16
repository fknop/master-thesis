package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.json.JsonSerializer
import village1.util.Utils

object CP_MIP_CPMIP_Benchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-mip${Utils.rand(0, 100)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val cp = benchmark.run(solveCP(benchmark))
  val mip = benchmark.run(solveMIP(benchmark))
  val cp_mip = benchmark.run(solveCP_MIP(benchmark))

  val instance = benchmark.makeInstance(("CP", cp), ("MIP", mip), ("CP_MIP", cp_mip))

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
