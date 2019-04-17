//package village1.search.cp.heuristic
//
//import oscar.algo.reversible.ReversibleInt
//import oscar.algo.search.Branching
//import oscar.cp.core.CPPropagStrength
//import oscar.cp.core.variables.CPVar
//import oscar.cp.{CPIntVar, Constraint}
//import oscar.cp.modeling.Branchings
//import village1.modeling.cp.VillageOneCPModel
//
//class RequirementFirstHeuristic(model: VillageOneCPModel) extends Branchings with Heuristic {
//
//
//  class VariableBoundListener(x: CPIntVar, reversible: ReversibleInt) extends Constraint(x.store) {
//    override def associatedVars(): Iterable[CPVar] = Array(x)
//
//    override def setup(l: CPPropagStrength): Unit = {
////      x.callPropagateWhenBind(this)
//    }
//
//    override def propagate(): Unit = {
//      reversible += 1
//    }
//  }
//
//
//  val x: Array[CPIntVar] = model.dual.variables.flatten.filter(_ != null)
//  val variables: Array[Array[CPIntVar]] = model.dual.variables
//
//  private val demands = model.demands
//  private val workers = model.workers
//  private val requirements = model.problem.workingRequirements
//  private val requiredWorkers: Map[Int, (Int, Int)] = requirements.foldLeft(Map[Int, (Int, Int)]()) { (acc, r) =>
//    acc.updated(r.worker, (r.min.getOrElse(0), r.max.getOrElse(workers(r.worker).availabilities.size)))}
//
//
//  private val reverseMap = buildReverseMap()
//
//  private val reversible = Array.tabulate(model.W)(w => if (requiredWorkers.contains(w)) new ReversibleInt(x(0).store, 0) else null)
//
//  private val mostAvailableFor: Array[Array[Int]] = Array.tabulate(model.W) { w =>
//    val nPositions = model.dual.nPositions
//    val sorted = (0 until nPositions).sortBy { p =>
//      val count = (for (t <- model.Periods if variables(t)(w) != null) yield variables(t)(w)).count(_.hasValue(p))
//      val remaining = workers(w).availabilities.size - count
//      (count)
//    }
//
//    sorted.toArray
//  }
//
//  for (i <- x.indices) {
//    val (_, w) = reverseMap(i)
//    if (requiredWorkers.contains(w)) {
//      x(i).callPropagateWhenBind(new VariableBoundListener(x(i), reversible(w)))
//    }
//  }
//
//
//  private def buildReverseMap(): Array[(Int, Int)] = {
//    var i = 0
//    val reverse = Array.fill[(Int, Int)](x.length)(null)
//    for (t <- model.Periods) {
//      for (w <- model.Workers if variables(t)(w) != null) {
//        reverse(i) = (t, w)
//        i += 1
//      }
//    }
//    reverse
//  }
//
//
//
//  def varHeuristic(i: Int): (Int, Int) = {
//    val (t, w) = reverseMap(i)
//
//    if (requiredWorkers.contains(w)) {
//      val (min, max) = requiredWorkers(w)
//      val occ = reversible(w).value
//      if (occ < min) {
//        (Int.MinValue, x(i).size)
//      }
//      else if (occ == max) {
//        (Int.MaxValue, x(i).size)
//      }
//      else {
//        (x(i).size, 0)
//      }
//    }
//    else {
//      (x(i).size, 0)
//    }
//  }
//
//
//  def mostAvailableHeuristic(i: Int): Int = {
//    val (_, w) = reverseMap(i)
//    val most = mostAvailableFor(w)
//    var j = 0
//    while (j < most.length) {
//      if (x(i).hasValue(most(j))) {
//        return most(j)
//      }
//
//      j += 1
//    }
//
//    x(i).max
//  }
//
//  def valueHeuristic(i: Int): Int = {
//
//    val (_, w) = reverseMap(i)
//    if (requiredWorkers.contains(w)) {
//      val (min, max) = requiredWorkers(w)
//      val occ = reversible(w).value
//      if (occ < min) {
//        mostAvailableHeuristic(i)
//      }
//      else {
//        x(i).min
//      }
//    }
//    else {
//      mostAvailableHeuristic(i)
//    }
//  }
//
//
//
//  def branching: Branching = binaryIdx(x, varHeuristic, valueHeuristic)
//}
