package village1.util

import village1.data.Demand

object Utilities {
  private val random = new scala.util.Random

  def generatePermutationsOfTwo (size: Int): IndexedSeq[(Int, Int)] = {
    for (i <- 0 until size; j <- i + 1 until size) yield (i, j)
  }

  /**
    * Returns the set of demands that overlap each other.
    * It will not return symmetries.
    * For example if set 0 overlaps with set 1, it will return that set 0 overlaps
    * with set 1 but not set 1 overlaps with set 0. This will avoid unnecessary constraints in the modeling.
    */
  def overlappingSets (demands: Array[Demand]): Array[Set[Int]] = {
    val sets = Array.tabulate(demands.length)(_ => Set[Int]())

    for (i <- demands.indices) {
      for (j <- i + 1 until demands.length) {
        if (demands(i).overlapWith(demands(j))) {
          sets(i) += j
        }
      }
    }

    sets
  }

  def rand(start: Int, end: Int): Int = start + random.nextInt((end - start) + 1)
}
