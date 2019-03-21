package village1.search.cp

import oscar.algo.search.Branching
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Branchings
import oscar.cp.searches.WeightedDegreeHelper
import village1.modeling.VillageOneModel

class MostAvailableHeuristic(model: VillageOneModel, x: Array[CPIntVar]) extends Branchings {

  // d -> (p -> (w0, w1, w2))
  private val mostAvailable: Array[Array[Array[Int]]] = generateMostAvailableWorkers()
  private val reverseMap = buildReverseMap()

  private def buildReverseMap(): Array[(Int, Int, Int)] = {
    var i = 0
    val reverse = Array.fill[(Int, Int, Int)](x.length)(null)
    for (t <- model.Periods) {
      for (d <- model.Demands if model.demands(d).hasPeriod(t)) {
        for (p <- model.demands(d).positions) {
          reverse(i) = (t, d, p)
          i += 1
        }
      }
    }
    reverse
  }

  private def generateMostAvailableWorkers(): Array[Array[Array[Int]]] = {
    val possible = model.possibleWorkersForDemands
    val workers = model.workers
    val demands = model.demands
    val mostAvailable = Array.fill[Array[Array[Int]]](model.D)(null)
    for (d <- model.Demands) {
      val pArray = Array.fill[Array[Int]](demands(d).nWorkers)(null)
      for (p <- demands(d).positions) {
        val possibleWorkers = demands(d).periods.foldLeft(Set[Int]())( (acc, t) => acc.union(possible(d)(t)(p)))

        val sorted = possibleWorkers.toArray.sortBy(w => {
          val worker = workers(w)
          worker.availabilities.intersect(demands(d).periods).size
        })(Ordering[Int].reverse)

        pArray(p) = sorted
      }

      mostAvailable(d) = pArray
    }

    mostAvailable
  }

  def varHeuristic(i: Int): Int = {
    val (_, d, p) = reverseMap(i)

   // Choose this variable if domain is 2: meaning it has one value and the sentinel value.
    if (x(i).size == 2) {
      Int.MinValue
    }
    else if (model.demands(d).requiredSkills.length > p) {
      maxDegree(x(i)) - model.demands(d).requiredSkills(p).length
    }
    else {
      maxDegree(x(i))
    }
  }

  def valueHeuristic(i: Int): Int = {
    val (_, d, p) = reverseMap(i)
    val mostAvailableWorkers = mostAvailable(d)(p)

    // If x(i) has one value and the sentinel value
    if (x(i).size == 2 && x(i).hasValue(-1)) {
      x(i).max
    }
    else {
      // Hot code, use while loop instead of for
      var j = 0
      while (j < mostAvailableWorkers.length) {
        if (x(i).hasValue(mostAvailableWorkers(j))) {
          return mostAvailableWorkers(j)
        }
        j += 1
      }

      x(i).min
    }
  }


  def branching: Branching = binaryIdx(x, varHeuristic, valueHeuristic)
}
