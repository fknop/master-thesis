package village1.benchmark

trait BenchmarkOptions {
  val solutionLimit: Int = Int.MaxValue
  val timeLimit: Int = 20
  val repeat: Int = 1
  val dryRun: Int = 1
  val noKeep: Boolean = false
  val T: Array[Int] = Array(5)
  val D: Array[Int] = Array(30, 50)
  val W: Array[Int] = Array(100, 200, 300)

  private def printArray(array: Array[Int]): String = s"(${array.mkString(", ")})"

  override def toString: String = {
    s"Benchmark created with: \nSolutionLimit=$solutionLimit\nTimeLimit=$timeLimit\nRepeat=$repeat\nDryRun=$dryRun" +
    s"\nT=${printArray(T)}\nD=${printArray(D)}\nW=${printArray(W)}"
  }
}