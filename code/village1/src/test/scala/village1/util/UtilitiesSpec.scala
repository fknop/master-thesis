package village1.util


import org.scalatest._
import village1.data.Demand

class UtilitiesSpec extends FunSpec with Matchers {

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
      val overlapping = Utilities.overlappingSets(demands)

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
      val permutations = Utilities.generatePermutationsOfTwo(5)

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
}
