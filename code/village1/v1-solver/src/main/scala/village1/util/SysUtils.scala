package village1.util

object SysUtils {

  /**
    * Returns the time passed to execute a block of code
    * @param block block of code
    * @return the time passed
    */
  def time(block: => Unit): Long = {
    val t0 = System.currentTimeMillis()
    block
    System.currentTimeMillis - t0
  }
}
