package village1.modeling.cp.constraints

import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.{CPModel, Constraint}

class SoftContiguous(x: Array[CPIntVar], y: CPIntVar) extends Constraint(y.store) with CPModel with oscar.cp.modeling.Constraints {
  override def associatedVars(): Iterable[CPVar] = x ++ Array(y)


  val intersections: Array[ReversibleInt] = Array.fill(x.length - 1)(null)
  val bound: Array[ReversibleBoolean] = Array.fill(x.length - 1)(new ReversibleBoolean(y.store, false))

  override def setup(l: CPPropagStrength): Unit = {
//    val bool: Array[CPIntVar] = Array.fill(x.length - 1)(null)
//    for (i <- 1 until x.length) {
//      bool(i - 1) = x(i).isDiff(x(i - 1))
//    }
//
//
//    s.post(sum(bool, y), CPPropagStrength.Strong)

    for (i <- 1 until x.length) {
      val intersection = x(i).toSet.intersect(x(i - 1).toSet)
      intersections(i - 1) = new ReversibleInt(y.store, intersection.size)
    }

    for (i <- x.indices if !x(i).isBound) {
      x(i).callValRemoveIdxWhenValueIsRemoved(this, x(i), i)
      x(i).callValBindIdxWhenBind(this, i)
    }
  }

  override def valBindIdx(variable: CPIntVar, idx: Int): Unit = {
    if (idx > 0) {
      if (x(idx).isBound && x(idx - 1).isBound && x(idx).value == x(idx - 1).value) {
        if (!bound(idx - 1).value) {
          y.updateMax(y.max - 1)
        }
        bound(idx - 1).setTrue()
      }
    }

    if (idx < x.length - 1) {
      if (x(idx).isBound && x(idx + 1).isBound && x(idx).value == x(idx + 1).value) {
        if (!bound(idx).value) {
          y.updateMax(y.max - 1)
        }
        bound(idx).setTrue()
      }
    }
  }

  override def valRemoveIdx(variable: CPIntVar, idx: Int, value: Int): Unit = {
    if (idx > 0) {
      if (x(idx - 1).hasValue(value)) {
        intersections(idx - 1) -= 1
        if (intersections(idx - 1).value == 0) {
          y.updateMin(y.min + 1)
        }
      }
    }


    if (idx < x.length - 1) {
      if (x(idx + 1).hasValue(value)) {
        intersections(idx) -= 1
        if (intersections(idx).value == 0) {
          y.updateMin(y.min + 1)
        }
      }
    }
  }
}
