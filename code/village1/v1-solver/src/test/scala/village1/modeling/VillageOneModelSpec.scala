package village1.modeling

import org.scalatest._
import village1.json.JsonParser
import village1.modeling

class VillageOneModelSpec extends FunSpec with Matchers {


  describe("Precomputed data (availabilities) should be correct") {

    val problem = JsonParser.parse("data/test/precompute.json")

    val precomputed = new modeling.VillageOneModel(problem)

    it("Should have correct availabilities precomputed") {
      val availabilities = precomputed.workersAvailabilities

      availabilities should have size 5
      availabilities.keys should contain only (0, 1, 2, 3, 4)

      val t0 = availabilities(0)
      t0 should have size 3
      t0 should contain only (0, 1, 3)

      val t1 = availabilities(1)
      t1 should have size 3
      t1 should contain only (0, 1, 2)

      val t2 = availabilities(2)
      t2 should have size 3
      t2 should contain only (0, 1, 2)

      val t3 = availabilities(3)
      t3 should have size 2
      t3 should contain only (2, 3)
    }

    it("Should have the correct precomputed availabilities for demands") {
      val availableWorkers = precomputed.availableWorkers

      availableWorkers should have size 3
      availableWorkers.keys should contain only (0, 1, 2)


      val d0 = availableWorkers(0)
      d0 should have size 3
      d0.keys should contain only (0, 1, 2)

      val d0t0 = d0(0)
      d0t0 should have size 3
      d0t0 should contain only (0, 1, 3)

      val d0t1 = d0(1)
      d0t1 should have size 3
      d0t1 should contain only (0 ,1, 2)

      val d0t2 = d0(2)
      d0t2 should have size 3
      d0t2 should contain only (0, 1, 2)



      val d1 = availableWorkers(1)
      d1 should have size 3
      d1.keys should contain only (1, 2, 3)

      val d1t1 = d1(1)
      d1t1 should have size 3
      d1t1 should contain only (0 ,1, 2)

      val d1t2 = d1(2)
      d1t2 should have size 3
      d1t2 should contain only (0, 1, 2)

      val d1t3 = d1(3)
      d1t3 should have size 2
      d1t3 should contain only (2, 3)


      val d2 = availableWorkers(2)
      d2 should have size 3
      d2.keys should contain only (0, 3, 4)

      val d2t0 = d2(0)
      d2t0 should have size 3
      d2t0 should contain only (0, 1, 3)

      val d2t3 = d2(3)
      d2t3 should have size 2
      d2t3 should contain only (2, 3)

      val d2t4 = d2(4)
      d2t4 should have size 1
      d2t4 should contain only 3
    }
  }

  describe("Precomputed data (skills) should be correct") {
    val problem = JsonParser.parse("data/test/precompute-skills.json")
    val precomputed = new modeling.VillageOneModel(problem)

    it("Should have the correct skills assigned to workers") {
      val skills = precomputed.workersWithSkills

      skills should have size 4
      skills.keys should contain only ("A", "B", "C", "D")

      skills("A") should have size 2
      skills("A") should contain only (0, 3)

      skills("B") should have size 2
      skills("B") should contain only (0, 1)

      skills("C") should have size 1
      skills("C") should contain only 2

      skills("D") should have size 2
      skills("D") should contain only (2, 3)
    }
  }

  describe("Precomputed data (possible workers for demand) should be correct") {
    val problem = JsonParser.parse("data/test/precompute-skills-demands-simple.json")
    val precomputed = new modeling.VillageOneModel(problem)


    it("Should have the correct workers assigned to each demand at each timeslot") {

      val possibleWorkers = precomputed.possibleWorkersForDemands

      possibleWorkers should have size 2
      possibleWorkers.keys should contain only (0, 1)

      val d0 = possibleWorkers(0)
      d0 should have size 1
      d0.keys should contain only 0

      val d1 = possibleWorkers(1)
      d1 should have size 1
      d1.keys should contain only 1

      val d0t0 = d0(0)
      d0t0 should have size 2

      val d0t0w0 = d0t0(0)
      d0t0w0 should have size 1
      d0t0w0 should contain only 0

      val d0t0w1 = d0t0(1)
      d0t0w1 should have size 2
      d0t0w1 should contain only (2, 4)

      val d1t1 = d1(1)
      d1t1 should have size 2

      val d1t1w0 = d1t1(0)
      d1t1w0 should have size 1
      d1t1w0 should contain only 5

      val d1t1w1 = d1t1(1)
      d1t1w1 should have size 3
      d1t1w1 should contain only (1, 3, 5)
    }
  }
}
