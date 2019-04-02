package village1.search.cp.heuristic

import oscar.algo.search.Branching
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Branchings
import village1.modeling.cp.VillageOneCPModel

class MostAvailableHeuristic(model: VillageOneCPModel, x: Array[CPIntVar], variables: Array[Array[Array[CPIntVar]]]) extends Branchings with Heuristic {

  private val demands = model.demands
  private val workers = model.workers
  private val mostAvailable: Array[Array[Array[Array[Int]]]] = generateMostAvailableWorkers()
  private val reverseMap = buildReverseMap()
  private val demandsAtTime: Array[Int] = buildDemandsAtTime()
  private val previous = buildPreviousPeriod()



  private def buildDemandsAtTime(): Array[Int] = {
    val demandsAtTime = Array.fill(model.T)(0)

    for (d <- model.Demands; t <- demands(d).periods) {
      demandsAtTime(t) += 1
    }

    demandsAtTime
  }


  private def buildReverseMap(): Array[(Int, Int, Int)] = {
    var i = 0
    val reverse = Array.fill[(Int, Int, Int)](x.length)(null)
    for (t <- model.Periods) {
      for (d <- model.Demands if demands(d).occurs(t)) {
        for (p <- demands(d).positions) {
          reverse(i) = (t, d, p)
          i += 1
        }
      }
    }
    reverse
  }

  private def buildPreviousPeriod(): Array[Array[Int]] = {
    val previous = Array.fill(model.D, model.T)(-1)

    for (d <- model.Demands) {
      val periods = demands(d).periods.toArray.sorted
      for (i <- periods.length - 1 until 0 by -1) {
        previous(d)(periods(i)) = periods(i - 1)
      }
    }

    previous
  }

  private def generateMostAvailableWorkers(): Array[Array[Array[Array[Int]]]] = {
    val possible = model.possibleWorkersForDemands
    val mostAvailable = Array.tabulate(model.D) { d =>
      Array.tabulate(model.demands(d).requiredWorkers) { p =>
        Array.fill[Array[Int]](model.T)(null)
      }
    }

    for (d <- model.Demands) {
      for (p <- demands(d).positions) {
        for (t <- demands(d).periods) {
          val possibleWorkers = possible(d)(t)(p)
          val sorted = possibleWorkers.toArray.sortBy(w => {
            val size = workers(w).availabilities.intersect(demands(d).periods).size
            val remaining = workers(w).availabilities.size - size
//            val requirement = model.problem.workingRequirements.find(_.worker == w)
//            var min = 0
//            var max = size
//            if (requirement.isDefined) {
//              max = requirement.get.max.getOrElse(0)
//              min = requirement.get.min.getOrElse(0)
//            }
            (-size, remaining, w)
          })(Ordering[(Int, Int, Int)])

          mostAvailable(d)(p)(t) = sorted
        }
      }
    }
    mostAvailable
  }

  def varHeuristic(i: Int): (Int) = {
    val (t, d, p) = reverseMap(i)

//    Choose this variable if domain is 2: meaning it has one value and the sentinel value.
    if (x(i).size == 2) {
      Int.MinValue //, demands(d).requiredWorkers, -demands(d).periods.size)
    }
    else {
      maxDegree(x(i)) - demands(d).requirements(p).skills.length //, demands(d).requiredWorkers, -demands(d).periods.size)
    }
//    i
  }

  def mostAvailableHeuristic(i: Int): Int = {

    val (t, d, p) = reverseMap(i)

    val mostAvailableWorkers = mostAvailable(d)(p)(t)

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


  def valueHeuristic(i: Int): Int = {
    val (t, d, p) = reverseMap(i)
    val prev = previous(d)(t)
    if (prev != -1 && variables(prev)(d)(p).isBound && x(i).hasValue(variables(prev)(d)(p).value)) {
        variables(prev)(d)(p).value
    }
    else {
      mostAvailableHeuristic(i)
    }
  }



  def branching: Branching = binaryIdx(x, varHeuristic, valueHeuristic)
}
