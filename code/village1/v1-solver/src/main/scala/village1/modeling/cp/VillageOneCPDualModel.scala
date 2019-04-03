package village1.modeling.cp

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import village1.modeling._
import village1.modeling.cp.constraints.AllDiffExcept

import scala.collection.mutable


class VillageOneCPDualModel(primalModel: VillageOneCPModel) extends VillageOneModel(primalModel.problem, Some(primalModel)) {

  implicit val solver = primalModel.solver

  type Variables = Array[Array[CPIntVar]]

  val positionMap: Array[(Int, Int, Int)] = precomputeIndices()
  val nPositions: Int = positionMap.length
  val possiblePositions = precomputePositionsForWorkers()

  val variables: Variables = generateVariables()

  def link(primal: Array[Array[Array[CPIntVar]]]): Unit = {
    val flatPrimal = primal.flatten.flatten

    val sentinel = CPIntVar(Set(Constants.SentinelWorker))

    var position = 0
    for (t <- Periods) {

//      for (w <- Workers if variables(t)(w) != null) {
//        val z = CPIntVar(Set(-1, w))
//        add(elementVar(sentinel +: flatPrimal, variables(t)(w) + 1, z), CPPropagStrength.Strong)
//        val withoutNull = variables(t).map(v => if (v == null) sentinel else v)
//        add(elementVar(sentinel +: withoutNull, z + 1, variables(t)(w)), CPPropagStrength.Strong)
//      }

      for (d <- Demands if demands(d).occurs(t)) {
        for (p <- demands(d).positions) {
          val z = CPIntVar(Set(-1, position))
          val withoutNull = variables(t).map(v => if (v == null) sentinel else v)
          add(elementVar(sentinel +: withoutNull, primal(t)(d)(p) + 1, z), CPPropagStrength.Strong)
          add(elementVar(sentinel +: flatPrimal, z + 1, primal(t)(d)(p)), CPPropagStrength.Strong)
          position += 1
        }
      }
    }

//    applyAllDifferent()
    add(gcc(variables.flatten.filter(_ != null), 0 until nPositions, 1, 1))
  }

  private def applyAllDifferent() = {
    for (w <- Workers) {
      val x = for (t <- Periods if variables(t)(w) != null) yield variables(t)(w)
      add(new AllDiffExcept(x, Set(-1)))
    }
  }

  def applyWorkingRequirements(): CPIntVar = {
    var violations = List[CPIntVar]()
    for (r <- problem.workingRequirements) {

      val workerVariables = for (t <- Periods if variables(t)(r.worker) != null) yield variables(t)(r.worker)

      val size = workerVariables.size
      val min = if (r.max.getOrElse(size) > size) 0 else size - r.max.getOrElse(size)
      val max = if (r.min.getOrElse(0) > size) 0 else size - r.min.getOrElse(0)

      val violation = CPIntVar(0, workers(r.worker).availabilities.size)


      add(softGcc(workerVariables, -1 to -1, Array(min), Array(max), violation), CPPropagStrength.Strong)

      violations ::= violation
    }

    if (violations.nonEmpty) {
      sum(violations)
    }
    else {
      null
    }
  }


  private def group(t: Int): List[Set[Int]] = {
    val taken = mutable.Set[Int]()
    var equivalent = List[Set[Int]]()

    for (w <- Workers if !taken.contains(w)) {

      val possibleW = possiblePositions(t)(w)
      val availabilitiesW = workers(w).availabilities

      var equivalentSet = Set[Int]()

      if (possibleW.nonEmpty) {
        for (w2 <- Workers if w != w2 && !taken.contains(w2)) {
          val possibleW2 = possiblePositions(t)(w2)
          val availabilitiesW2 = workers(w2).availabilities

          if (possibleW == possibleW2 && availabilitiesW == availabilitiesW2) {
            taken.add(w)
            taken.add(w2)
            equivalentSet += w
            equivalentSet += w2
          }
        }
      }

      if (equivalentSet.nonEmpty) {
        equivalent ::= equivalentSet
      }

    }

    equivalent
  }

  private def generateVariables(): Variables = {
    Array.tabulate(T, W) { (t, w) =>
      val possible = possiblePositions(t)(w)
      if (possible.isEmpty) null else CPIntVar(possible.union(Set(-1)))
    }
  }

  private def precomputePositionsForWorkers(): Array[Array[Set[Int]]] = {
    val result = Array.fill(T, W)(Set[Int]())
    for (w <- Workers) {
      for (i <- positionMap.indices) {
        val (t, d, p) = positionMap(i)
        if (possibleWorkersForDemands(d)(t)(p).contains(w)) {
          result(t)(w) = result(t)(w) + i
        }
      }
    }

    result
  }

  private def precomputeIndices(): Array[(Int, Int, Int)] = {
    val positions = Array.fill[(Int, Int, Int)](demands.map(d => d.requiredWorkers * d.periods.size).sum)(null)
    var position = 0
    for (i <- Periods) {
      for (d <- Demands if demands(d).occurs(i)) {
        for (p <- demands(d).positions) {
          positions(position) = (i, d, p)
          position += 1
        }
      }
    }

    positions
  }

}
