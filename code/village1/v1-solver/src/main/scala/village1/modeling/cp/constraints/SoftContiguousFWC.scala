package village1.modeling.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.{CPModel, Constraint}

class SoftContiguousFWC(x: Array[CPIntVar], y: CPIntVar) extends Constraint(y.store) with CPModel with oscar.cp.modeling.Constraints {
  override def associatedVars(): Iterable[CPVar] = x ++ Array(y)

  override def setup(l: CPPropagStrength): Unit = {

    for (xi <- x if !xi.isBound) {
      xi.callPropagateWhenBind(this)
    }

    propagate()
  }

  override def propagate(): Unit = {
    var i = 1
    var diff = 0
    var allBound = true

    while (i < x.length && allBound) {
      if (!x(i).isBound || !x(i - 1).isBound) {
        allBound = false
      }
      else {
        if (x(i).value != x(i - 1).value) {
          diff += 1
        }
      }
      i += 1
    }

    if (allBound) {
      y.assign(diff)
    }
  }
}
