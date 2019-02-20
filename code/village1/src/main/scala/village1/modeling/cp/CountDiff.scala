package village1.modeling.cp

import oscar.cp.constraints.AtMostNValue
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import oscar.cp.{CPIntVar, Constraint}


class CountDifferent(val X: Array[CPIntVar], val Y: CPIntVar) extends Constraint(Y.store, "CountSimple") {

  override def associatedVars(): Iterable[CPVar] = Array(Y) ++ X

  override def setup(l: CPPropagStrength): Unit = {
    X.foreach(_.callPropagateWhenDomainChanges(this))
  }

  override def propagate(): Unit = {
    // TODO
  }
}


