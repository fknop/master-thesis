package village1.util

object Benchmark {
  def time(block: => Unit): Long = {
    val t0 = System.currentTimeMillis()
    block
    System.currentTimeMillis - t0
  }
}
