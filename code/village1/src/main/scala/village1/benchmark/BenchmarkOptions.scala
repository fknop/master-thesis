package village1.benchmark

trait BenchmarkOptions {
  val solutionLimit: Int = Int.MaxValue
  val timeLimit: Int = 20
  val repeat: Int = 1
  val dryRun: Int = 1
  val noKeep: Boolean = false
}