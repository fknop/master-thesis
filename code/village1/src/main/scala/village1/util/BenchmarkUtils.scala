package village1.util

object BenchmarkUtils {

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

  import Numeric.Implicits._

  /**
    * Computes the mean of a list of values
    * @param xs the list of values
    * @tparam T the numeric type
    * @return the mean of the list of values
    */
  def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

  /**
    * Computes the variance of a list of values
    * @param xs the list of values
    * @tparam T the numeric type
    * @return the variance of the list of values
    */
  def variance[T: Numeric](xs: Iterable[T]): Double = {
    val avg = mean(xs)

    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  /**
    * Computes the standard deviation of a list of values
    * @param xs the list of values
    * @tparam T the numeric type
    * @return the standard deviation of the list of values
    */
  def stdDev[T: Numeric](xs: Iterable[T]): Double = math.sqrt(variance(xs))
}