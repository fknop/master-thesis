package village1.benchmark

import BenchmarkSolverFunctions._
import village1.json.JsonSerializer

object SymmetryBreakingBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = "data/benchmark/cp-symmetry.json"))

  println(options)

  val benchmark = new SolverBenchmark(options = options)
  val cpResults = benchmark.run(solveCP(benchmark))
  val cpResultsSB = benchmark.run(solveCPWithSymmetries(benchmark))

  val instance = benchmark.makeInstance(("CP-NoSym", cpResults), ("CP-Sym", cpResultsSB))

  val writer = JsonSerializer.serialize(instance)
  writer(options.out)
}
