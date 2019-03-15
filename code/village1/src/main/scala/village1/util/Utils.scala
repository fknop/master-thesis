package village1.util

import village1.data.Demand

import scala.collection.mutable

object Utils {
  private val random = new scala.util.Random

  /**
    * Generate permutations of two numbers
    * Only generate them in one way
    * For example: Size 3: will generate (0, 1), (0, 2), (1, 2)
    * @param n the maximum value (excluded)
    * @return permutations of all numbers between [0, n[
    */
  def generatePermutationsOfTwo (n: Int): IndexedSeq[(Int, Int)] = {
    for (i <- 0 until n; j <- i + 1 until n) yield (i, j)
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

  /**
    * Return a random integer value between [start, end]
    * @param start the start value
    * @param end the end value
    * @return a random integer value between [start, end]
    */
  def rand (start: Int, end: Int): Int = start + random.nextInt((end - start) + 1)

  def groupByEquality (sets: Array[Set[Int]]): List[Set[Int]] = {
    val used = mutable.Set[Int]()
    var groups = List[Set[Int]]()

    for (i <- sets.indices) {
      if (!used.contains(i)) {
        val equal = mutable.Set[Int]()
        for (j <- i + 1 until sets.length) {
          if (!used.contains(j) && sets(i) == sets(j)) {
            used.add(i)
            used.add(j)
            equal.add(i)
            equal.add(j)
          }
        }

        if (equal.nonEmpty) {
          groups = equal.toSet :: groups
        }
      }
    }

    groups
  }

  def removeSymmetries(sets: Array[Set[Int]], groups: List[Set[Int]]): Array[Set[Int]] = {
    val finalSets = Array.tabulate(sets.length)(sets(_))
    for (group <- groups.map(_.toArray)) {
      val groupSize = group.length
      val n = sets(group(0)).size

      val subGroupSize = n / groupSize
      val values = sets(group(0)).toArray.sorted
      val subGroupSets = Array.fill(groupSize)(Set[Int]())

      var i = 0
      var j = 0
      while (i < groupSize) {
        while (j < (subGroupSize * (i + 1)) || (i == groupSize - 1 && j < values.length)) {
          subGroupSets(i) += values(j)
          j += 1
        }

        finalSets(group(i)) = subGroupSets(i)
        i += 1
      }
    }

    finalSets
  }

  def removeSymmetries(sets: Array[Set[Int]]): Array[Set[Int]] = {
    removeSymmetries(sets, groupByEquality(sets))
  }
}
