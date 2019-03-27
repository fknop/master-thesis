package village1.modeling.cp.constraints

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import village1.modeling._

class SoftContiguousSpec extends CommonSpec {
  describe("Should return correct sum of differences") {
    implicit val s = CPSolver()
    val w = CPIntVar(Set(1, 2, 3))
    val x = CPIntVar(Set(2, 3, 4, 6))
    val y = CPIntVar(Set(3, 4, 5, 6))
    val z = CPIntVar(Set(5, 6, 7))

    val diff = CPIntVar(0, 3)

    val ctr = new SoftContiguous(Array(w, x, y, z), diff)
    s.post(ctr)

    it("should have [0, 3] bounds") {
      diff.size should equal(4)
      diff.min should equal(0)
      diff.max should equal(3)
    }

    it("should have [1, 3] bounds") {
      x.removeValue(2)
      x.removeValue(3)
      s.propagate(ctr)
      diff.min should equal(1)
      diff.max should equal(3)
    }

    it("should have [1, 2] bounds") {
      y.assign(6)
      z.assign(6)
      s.propagate(ctr)
      diff.min should equal(1)
      diff.max should equal(2)
    }

    it("should be bound to 2") {
      x.assign(4)
      s.propagate(ctr)
      diff.isBound should equal(true)
      diff.value should equal(2)
    }
  }

  describe("Should return correct sum of differences (0)") {
    implicit val s = CPSolver()
    val w = CPIntVar(Set(1, 2, 3))
    val x = CPIntVar(Set(2, 3, 4, 6))
    val y = CPIntVar(Set(3, 4, 5, 6))
    val z = CPIntVar(Set(3, 5, 6, 7))

    val diff = CPIntVar(0, 3)

    val ctr = new SoftContiguous(Array(w, x, y, z), diff)
    s.post(ctr)

    it("should have [0, 3] bounds") {
      w.assign(3)
      x.assign(3)
      y.assign(3)
      z.assign(3)
      s.propagate(ctr)
      diff.isBound should equal(true)
      diff.value should equal(0)
    }

  }
}
