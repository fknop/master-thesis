package village1.modeling.cp

import org.scalatest._
import oscar.cp.core.NoSolutionException
import village1.json.JsonParser
import village1.modeling.{Problem, UnsolvableException}
import village1.search.cp.VillageOneSearch

class VillageOneCPModelSpec extends FunSpec with Matchers {

  private def getSearch(problem: Problem): VillageOneSearch = {
    new VillageOneSearch(problem)
  }

  describe("Additional skills for demands") {
    it("Should return the correct workers assigned with additional skills") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills.json"))

      search.onSolutionFound { solution =>
        solution.valid should be (true, "OK")
        val plannings = solution.plannings
        plannings should have size 2

        val p1 = plannings(1)
        val workers = p1.workerAssignments.head.workers
        workers should have size 1
        workers(0) should not equal 0

        val p0 = plannings(0)
        val workers0 = p0.workerAssignments.head.workers
        workers0 should have size 2
        List(workers0(0), workers0(1)) should contain (0)
      }

      search.solve()
    }

    it("Should return the correct workers assigned with additional skills (with special Max value)") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-value.json"))

      search.onSolutionFound { solution =>
        solution.valid should be (true, "OK")
        val plannings = solution.plannings
        val p = plannings(0)

        val workers = p.workerAssignments.head.workers
        workers should have size 3
      }

      search.solve()
    }

    it("Should return the correct workers assigned with additional skills (with special Min value)") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-value-2.json"))

      search.onSolutionFound { solution =>
        solution.valid should be (true, "OK")
        val plannings = solution.plannings
        val p = plannings(0)

        val workers = p.workerAssignments.head.workers
        workers should have size 3
      }

      search.solve()
    }

    it("Should be unsolvable") {
      an [NoSolutionException] should be thrownBy {
        getSearch(JsonParser.parse("data/test/additional-skills-impossible.json"))
      }
    }

    it("Should be unsolvable - 2") {
      an [UnsolvableException] should be thrownBy {
        getSearch(JsonParser.parse("data/test/additional-skills-impossible2.json"))
      }
    }
  }

  describe("Machine assignments") {
    it("Should return the correct machine assignments") {
      val search = getSearch(JsonParser.parse("data/test/machines-assignment.json"))

      search.onSolutionFound { solution =>
        solution.valid should be (true, "OK")

        val plannings = solution.plannings
        plannings should have size 2

        val p0 = plannings(0)
        val p1 = plannings(1)

        p0.machineAssignments.get should contain (0)
        p0.machineAssignments.get should contain oneOf (1, 2)

        p1.machineAssignments.get should contain oneOf (1, 2)

        if (p0.machineAssignments.get.contains(1)) {
          p1.machineAssignments.get should contain (2)
        }
        else {
          p0.machineAssignments.get should contain (2)
          p1.machineAssignments.get should contain (1)
        }
      }

      search.solve()
    }
  }

  describe("Location assignments") {
    it("Should have the same location assigned") {
      val search = getSearch(JsonParser.parse("data/test/locations-assignment.json"))

      search.onSolutionFound { solution =>
        solution.valid should be (true, "OK")

        val plannings = solution.plannings
        plannings should have size 2

        val p0 = plannings(0)
        val p1 = plannings(1)

        p0.locationAssignment.get should equal(0)
        p1.locationAssignment.get should equal(0)
      }

      search.solve()
    }

    it("Should have different location assigned") {
      val search = getSearch(JsonParser.parse("data/test/locations-assignment2.json"))

      search.onSolutionFound { solution =>
        solution.valid should be (true, "OK")

        val plannings = solution.plannings
        plannings should have size 2

        val p0 = plannings(0)
        val p1 = plannings(1)

        val l0 = p0.locationAssignment.get
        val l1 = p1.locationAssignment.get

        if (l0 == 0) {
          l0 should equal(0)
          l1 should equal(1)
        }
        else {
          l0 should equal(1)
          l1 should equal(0)
        }
      }

      search.solve()
    }
  }
}
