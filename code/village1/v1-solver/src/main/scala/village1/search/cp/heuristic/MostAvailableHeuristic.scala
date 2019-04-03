package village1.search.cp.heuristic

import oscar.algo.search.Branching
import oscar.cp.core.variables.CPIntVar
import oscar.cp.modeling.Branchings
import village1.modeling.cp.VillageOneCPModel

class MostAvailableHeuristic(model: VillageOneCPModel, x: Array[CPIntVar], variables: Array[Array[Array[CPIntVar]]]) extends Branchings with Heuristic {

  private val demands = model.demands
  private val workers = model.workers
  private val mostAvailable: Array[Array[Array[Array[Int]]]] = generateMostAvailableWorkers()
  private val mostAvailableWithRequirements: Array[Array[Array[Array[Int]]]] = generateMostAvailableWorkersWithRequirements()
  private val reverseMap = buildReverseMap()
  private val previous = buildPreviousPeriod()

  var useRequirements: Boolean = false

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
            val requirement = model.problem.workingRequirements.find(_.worker == w)
            var min = 0
            var max = workers(w).availabilities.size
            if (requirement.isDefined) {
              max = requirement.get.max.getOrElse(max)
              min = requirement.get.min.getOrElse(min)
            }
            val size = math.min(workers(w).availabilities.intersect(demands(d).periods).size, max)
            val remaining = max - size
            (-size, remaining, w)
          })(Ordering[(Int, Int, Int)])

          mostAvailable(d)(p)(t) = sorted
        }
      }
    }
    mostAvailable
  }

  private def generateMostAvailableWorkersWithRequirements(): Array[Array[Array[Array[Int]]]] = {
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
            val requirement = model.problem.workingRequirements.find(_.worker == w)
            var min = 0
            var max = size
            if (requirement.isDefined) {
              max = requirement.get.max.getOrElse(0)
              min = requirement.get.min.getOrElse(0)
            }
            (-min, -size, remaining, w)
          })(Ordering[(Int, Int, Int, Int)])

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
      Int.MinValue
    }
    else {
      maxDegree(x(i)) - demands(d).requirements(p).skills.length
    }
  }

  def mostAvailableHeuristic(i: Int): Int = {

    val (t, d, p) = reverseMap(i)

    val mostAvailableWorkers =
      if (useRequirements) mostAvailableWithRequirements(d)(p)(t)
      else mostAvailable(d)(p)(t)

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


  val last = new LastAssignedHeuristic(x)
  def branching: Branching = binaryIdx(x, varHeuristic, last.valueHeuristic(valueHeuristic))
}
