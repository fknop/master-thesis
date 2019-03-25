package village1.util


import org.scalatest._
import village1.data.Demand

class UtilsSpec extends FunSpec with Matchers {

  describe("Overlapping sets") {

    val periodsList = Array(
      Set(0, 1, 2),
      Set(1, 2, 3),
      Set(3, 4, 5),
      Set(5, 6, 7),
      Set(8, 9, 10),
      Set(0)
    )

    val demands = periodsList.indices.map( i =>
      Demand(id = i, client = i, periods = periodsList(i), requiredWorkers = i)
    ).toArray

    it("Should return the correct overlapping sets") {
      val overlapping = Utils.overlappingSets(demands)

      overlapping should have size 6

      overlapping(0) should have size 2
      overlapping(0) should contain only (1, 5)

      overlapping(1) should have size 1
      overlapping(1) should contain only 2

      overlapping(2) should have size 1
      overlapping(2) should contain only 3

      overlapping(3) should have size 0

      overlapping(4) should have size 0

      overlapping(5) should have size 0
    }

  }

  describe("Permutations of two") {

    it("Should return the correct permutations") {
      val permutations = Utils.generatePermutationsOfTwo(5)

      permutations should contain only (
        (0, 1),
        (0, 2),
        (0, 3),
        (0, 4),
        (1, 2),
        (1, 3),
        (1, 4),
        (2, 3),
        (2, 4),
        (3, 4)
      )
    }
  }

  describe("groupByEquality") {
    it("Should group set correctly by their equality") {
      val a = Set(1, 2, 3) // with f and e
      val b = Set(1, 2, 3, 4) // with g
      val c = Set(1, 2, 5) // alone
      val d = Set(1, 2, 3) // with a and f
      val e = Set(1, 2, 4) // alone
      val f = Set(3, 2, 1) // with a and d
      val g = Set(1, 2, 3, 4) // with b

      val groups = Utils.groupByEquality(Array(a, b, c, d, e, f, g))
      groups should have size 2
      groups(0) should have size 2
      groups(1) should have size 3

      groups(0) should contain only(1, 6)
      groups(1) should contain only (0, 3, 5)
    }
  }

  describe("removeSymmetries") {

    val a = Set(1, 2, 3, 4)
    val b = Set(4, 3, 1, 2)

    val c = Set(1, 2, 3, 4, 5)
    val d = Set(5, 4, 3, 2, 1)

    it("Should split symmetries (even sets)") {
      val sets = Utils.removeSymmetries(Array(a, b))
      sets(0) should have size 2
      sets(1) should have size 2

      sets(0) should contain only (1, 2)
      sets(1) should contain only (3, 4)
    }

    it("Should split symmetries (odd sets)") {
      val sets = Utils.removeSymmetries(Array(c, d))
      sets(0) should have size 2
      sets(1) should have size 3

      sets(0) should contain only (1, 2)
      sets(1) should contain only (3, 4, 5)
    }

    it("Should split symmetries (both even and odd sets)") {
      val sets = Utils.removeSymmetries(Array(a, b, c, d))
      sets(0) should have size 2
      sets(1) should have size 2

      sets(2) should have size 2
      sets(3) should have size 3

      sets(0) should contain only (1, 2)
      sets(1) should contain only (3, 4)

      sets(2) should contain only (1, 2)
      sets(3) should contain only (3, 4, 5)
    }

    it("Should split symmetries (unused sets)") {
      val sets = Utils.removeSymmetries(Array(a, c, b))
      sets(0) should have size 2
      sets(1) should have size 5
      sets(2) should have size 2

      sets(0) should contain only (1, 2)
      sets(2) should contain only (3, 4)

      sets(1) should contain only (1, 2, 3, 4, 5)

    }
  }
}
