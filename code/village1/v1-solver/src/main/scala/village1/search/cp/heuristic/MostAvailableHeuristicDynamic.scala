package village1.search.cp.heuristic

import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.algo.search.Branching
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.modeling.Branchings
import village1.modeling.Constants
import village1.modeling.cp.VillageOneCPModel

class MostAvailableHeuristicDynamic(model: VillageOneCPModel, x: Array[CPIntVar], variables: Array[Array[Array[CPIntVar]]]) extends Branchings with Heuristic {

  class OnBindConstraint(x: Array[CPIntVar]) extends Constraint(x(0).store) {
    override def associatedVars(): Iterable[CPVar] = x

    override def setup(l: CPPropagStrength): Unit = {
      for (i <- x.indices) {
        if (x(i).isBound)
          updateCounters(i)
        else
          x(i).callValBindIdxWhenBind(this, i)
      }
    }

    override def valBindIdx(variable: CPIntVar, i: Int) {
      updateCounters(i)
    }

    private def updateCounters(i: Int): Unit = {
      val (t, d, p) = reverseMap(i)
      val w = x(i).value
      if (w != Constants.SentinelWorker) {
        occurrences(w) += 1
        _occurrencesForPosition(w)(d)(p) += 1
        _availabilities(w).removeValue(t)
      }
      _periods(d)(p).removeValue(t)
    }
  }

  private val demands = model.demands
  private val workers = model.workers

  private val occurrences = Array.fill[ReversibleInt](model.W)(new ReversibleInt(x(0).store, 0))
  private val _availabilities = Array.tabulate[ReversibleSparseSet](model.W)(w => new ReversibleSparseSet(x(0).store,
    workers(w).availabilities.min, workers(w).availabilities.max
  ))

  private val _periods = Array.tabulate(model.D) { d =>
    Array.tabulate(demands(d).requiredWorkers) { p =>
      val periods = demands(d).periods
      new ReversibleSparseSet(x(0).store, periods.min, periods.max)
    }
  }

  private val _occurrencesForPosition = Array.tabulate(model.W, model.D) { (w, d) =>
    Array.tabulate(demands(d).requiredWorkers) { p =>
      new ReversibleInt(x(0).store, 0)
    }
  }

  private val _tmp = Array.fill(model.T)(0)

  private val _requirements: Array[(Int, Int)] = Array.tabulate(model.W) { w =>
    val requirement = model.problem.workingRequirements.find(_.worker == w)
    if (requirement.isDefined) {
      val min = requirement.get.min.getOrElse(0)
      val max = requirement.get.max.getOrElse(workers(w).availabilities.size)
      (min, max)
    }
    else {
      (0, workers(w).availabilities.size)
    }
  }

  var mostAvailable: Array[Array[Array[Array[Int]]]] = generateMostAvailableWorkers()
//  private val mostAvailableWithRequirements: Array[Array[Array[Array[Int]]]] = generateMostAvailableWorkersWithRequirements()

  private val reverseMap = buildReverseMap()
  private val previous = buildPreviousPeriod()
  var useRequirements: Boolean = false


  for (w <- model.Workers) {
    for (v <- _availabilities(w).min to _availabilities(w).max) {
      if (!workers(w).availabilities.contains(v)) {
        _availabilities(w).removeValue(v)
      }

    }
    assert(_availabilities(w).size == workers(w).availabilities.size)
  }

  for (d <- model.Demands; p <- demands(d).positions) {
    for (v <- _periods(d)(p).min to _periods(d)(p).max) {
      if (!demands(d).periods.contains(v)) {
        _periods(d)(p).removeValue(v)
      }
    }

    assert(_periods(d)(p).size == demands(d).periods.size)
  }

  x(0).store.add(new OnBindConstraint(x))


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
          mostAvailable(d)(p)(t) = possible(d)(t)(p).toArray
        }
      }
    }

    mostAvailable
  }
//
//
//  private def generateMostAvailableWorkersWithRequirements(): Array[Array[Array[Array[Int]]]] = {
//    val possible = model.possibleWorkersForDemands
//    val mostAvailable = Array.tabulate(model.D) { d =>
//      Array.tabulate(model.demands(d).requiredWorkers) { p =>
//        Array.fill[Array[Int]](model.T)(null)
//      }
//    }
//
//    for (d <- model.Demands) {
//      for (p <- demands(d).positions) {
//        for (t <- demands(d).periods) {
//          mostAvailable(d)(p)(t) = possible(d)(t)(p).toArray
//        }
//      }
//    }
//
//    mostAvailable
//  }

  def varHeuristic(i: Int): Int = {
    val (t, d, p) = reverseMap(i)

//    Choose this variable if domain is 2: meaning it has one value and the sentinel value.
    if (x(i).size == 2) {
      Int.MinValue
    }
    else {
      maxDegree(x(i)) - demands(d).requirements(p).skills.length
    }
  }

  private def intersectionSize(r: ReversibleSparseSet, s: Set[Int]): Int = {
    if (r.isEmpty || s.isEmpty) 0
    else {
      var size = 0
      for (v <- s) {
        if (r.hasValue(v)) {
          size += 1
        }
      }

      size
    }
  }

  private def intersectionSize(r: ReversibleSparseSet, r2: ReversibleSparseSet): Int = {
    if (r.isEmpty || r2.isEmpty) 0
    else {
      var size = 0
      val rSize = r.fillArray(_tmp)
      var i = 0
      while (i < rSize) {
        if (r2.hasValue(_tmp(i))) {
          size += 1
        }
        i += 1
      }

      size
    }
  }

  import scala.math.Ordering.Implicits._
  def mostAvailableHeuristic(i: Int): Int = {

    val (t, d, p) = reverseMap(i)

    var j = 0
    var minWorker = Constants.SentinelWorker
    var minValue: (Int, Int, Int, Int, Int) = null
    val possible = mostAvailable(d)(p)(t)
    while (j < possible.length) {
      val w = possible(j)
      val (min, max) = _requirements(w)
      val remainingMax = max - occurrences(w).value

      val intersection = intersectionSize(_availabilities(w), demands(d).periods)
      val size = math.min(intersection, remainingMax)
      val remaining = remainingMax - size
      val occ = _occurrencesForPosition(w)(d)(p)
      val impossible = max == occurrences(w).value || !x(i).hasValue(w)

      if (!impossible) {
        val value =
          if (useRequirements)
            (math.max(-(min - occurrences(w).value), 0), -occ, -size, remaining, w)
          else
            (-occ, -size, remaining, w, 0)

        if (minValue == null || value < minValue) {
          minValue = value
          minWorker = w
        }
      }

      j += 1
    }

    if (x(i).hasValue(minWorker)) minWorker else x(i).max
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
