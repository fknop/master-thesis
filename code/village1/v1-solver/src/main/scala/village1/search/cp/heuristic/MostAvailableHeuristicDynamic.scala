package village1.search.cp.heuristic

import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.Branching
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.modeling.Branchings
import village1.modeling.cp.VillageOneCPModel

class MostAvailableHeuristicDynamic(model: VillageOneCPModel, x: Array[CPIntVar], variables: Array[Array[Array[CPIntVar]]]) extends Branchings with Heuristic {

  private val demands = model.demands
  private val workers = model.workers

  private val possiblePositions: Array[Array[(Int, Int)]] = generatePossiblePositions()

  private val occurrences = Array.fill[ReversibleInt](model.W)(new ReversibleInt(x(0).store, 0))

  var mostAvailable: Array[Array[Array[Int]]] = generateMostAvailableWorkers()

  private val mostAvailableWithRequirements: Array[Array[Array[Int]]] = generateMostAvailableWorkersWithRequirements()
  private val reverseMap = buildReverseMap()
  private val previous = buildPreviousPeriod()
  var useRequirements: Boolean = false




  for (i <- x.indices) {
    if (x(i).isBound) {
      occurrences(x(i).value) += 1
    }
    else {
      x(i).callPropagateWhenBind(new Constraint(x(i).store) {
        override def associatedVars(): Iterable[CPVar] = Array(x(i))

        override def setup(l: CPPropagStrength): Unit = {}

        override def propagate(): Unit = {
          val w = x(i).value
          occurrences(w) += 1
//          val (t, d, p) = reverseMap(i)
//          if (demands(d).periods.size < 5 && occurrences(w).value < workers(w).availabilities.size) {
//            for ((d, p) <- possiblePositions(w)) {
//              mostAvailable(d)(p) = mostAvailable(d)(p).sortBy(w => {
//                val requirement = model.problem.workingRequirements.find(_.worker == w)
//                var min = 0
//                var max = workers(w).availabilities.size
//                if (requirement.isDefined) {
//                  max = requirement.get.max.getOrElse(max)
//                  min = requirement.get.min.getOrElse(min)
//                }
//                val size = math.min(workers(w).availabilities.intersect(demands(d).periods).size, max) - occurrences(w).value
//                val remaining = max - size
//                (-size, remaining, w)
//              })(Ordering[(Int, Int, Int)])
//            }
//          }
        }
      })
    }
  }

  private def generatePossiblePositions(): Array[Array[(Int, Int)]] = {
    val results = Array.fill[Array[(Int, Int)]](model.W)(null)

    val possibleWorkers = model.possibleWorkersForDemands
    for (w <- model.Workers) {
      var set = Set[(Int, Int)]()
      for ((d, v) <- possibleWorkers; (t, v2) <- v; p <- v2.indices) {
        if (v2(p).contains(w)) {
          set += ((d, p))
        }
      }
      results(w) = set.toArray
    }

    results
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

  private def generateMostAvailableWorkers(): Array[Array[Array[Int]]] = {
    val possible = model.possibleWorkersForDemands
    val mostAvailable = Array.tabulate(model.D) { d =>
      Array.fill[Array[Int]](model.demands(d).requiredWorkers)(null)
    }

    for (d <- model.Demands) {
      for (p <- demands(d).positions) {
          val possibleWorkers = demands(d).periods.map(t => possible(d)(t)(p)).reduce((a,b) => a.union(b))
          val sorted = possibleWorkers.toArray.sortBy(w => {
            val requirement = model.problem.workingRequirements.find(_.worker == w)
            var min = 0
            var max = workers(w).availabilities.size
            if (requirement.isDefined) {
              max = requirement.get.max.getOrElse(max)
              min = requirement.get.min.getOrElse(min)
            }
            val size = math.min(workers(w).availabilities.intersect(demands(d).periods).size, max) - occurrences(w).value
            val remaining = max - size
            (-size, remaining, w)
          })(Ordering[(Int, Int, Int)])

          mostAvailable(d)(p) = sorted

      }
    }
    mostAvailable
  }


  private def generateMostAvailableWorkersWithRequirements(): Array[Array[Array[Int]]] = {
    val possible = model.possibleWorkersForDemands
    val mostAvailable = Array.tabulate(model.D) { d =>
      Array.fill[Array[Int]](model.demands(d).requiredWorkers)(null)
    }

    for (d <- model.Demands) {
      for (p <- demands(d).positions) {
        val possibleWorkers = demands(d).periods.map(t => possible(d)(t)(p)).reduce((a,b) => a.union(b))

        val sorted = possibleWorkers.toArray.sortBy(w => {
          val requirement = model.problem.workingRequirements.find(_.worker == w)
          var min = 0
          var max = workers(w).availabilities.size
          if (requirement.isDefined) {
            max = requirement.get.max.getOrElse(max)
            min = requirement.get.min.getOrElse(min)
          }
          val size = math.min(workers(w).availabilities.intersect(demands(d).periods).size, max) - occurrences(w).value
          val remaining = max - size

          (math.max(-(min - occurrences(w).value), 0), -size, remaining, w)
        })(Ordering[(Int, Int, Int, Int)])

        mostAvailable(d)(p) = sorted
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

    if (!useRequirements) {
      mostAvailable(d)(p) = mostAvailable(d)(p).sortBy(w => {
        val requirement = model.problem.workingRequirements.find(_.worker == w)
        var min = 0
        var max = workers(w).availabilities.size
        if (requirement.isDefined) {
          max = requirement.get.max.getOrElse(max)
          min = requirement.get.min.getOrElse(min)
        }
        val size = math.min(workers(w).availabilities.intersect(demands(d).periods).size, max) - occurrences(w).value
        val remaining = max - size
        (-size, remaining, w)
      })(Ordering[(Int, Int, Int)])
    }
    else {
      mostAvailableWithRequirements(d)(p) = mostAvailableWithRequirements(d)(p).sortBy(w => {
        val requirement = model.problem.workingRequirements.find(_.worker == w)
        var min = 0
        var max = workers(w).availabilities.size
        if (requirement.isDefined) {
          max = requirement.get.max.getOrElse(max)
          min = requirement.get.min.getOrElse(min)
        }
        val size = math.min(workers(w).availabilities.intersect(demands(d).periods).size, max) - occurrences(w).value
        val remaining = max - size

        (-size, math.max(-(min - occurrences(w).value), 0), remaining, w)
      })(Ordering[(Int, Int, Int, Int)])
    }

    val mostAvailableWorkers =
      if (useRequirements) mostAvailableWithRequirements(d)(p)
      else mostAvailable(d)(p)


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
