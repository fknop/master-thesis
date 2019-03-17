package village1.benchmark


import village1.benchmark.BenchmarkSolverFunctions._
import village1.json.JsonSerializer
import village1.util.Utils

object CPMIPBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-mip${Utils.randomInt(0, 100)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val cp = benchmark.run("CP", solveCP(benchmark))
  val mip = benchmark.run("MIP", solveMIP(benchmark))

  val instance = benchmark.makeInstance(cp, mip)

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
