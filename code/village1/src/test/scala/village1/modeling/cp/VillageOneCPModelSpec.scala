package village1.modeling.cp

import org.scalatest.Inside.inside
import org.scalatest._
import oscar.cp.core.NoSolutionException
import village1.json.JsonParser
import village1.modeling._
import village1.modeling.violations.{WorkerViolation, WorkingRequirementViolation}
import village1.search.cp.VillageOneSearch

class VillageOneCPModelSpec extends CommonSpec {

  private def getSearch(problem: Problem, options: CPModelOptions = CPModelOptions()): VillageOneSearch = {
    new VillageOneSearch(problem, options)
  }


  describe("Additional skills for demands") {
    it("Should return the correct workers assigned with additional skills") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills.json"))

      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)
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
        checkValid(solution)
        checkNotPartial(solution)

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
        checkValid(solution)
        checkNotPartial(solution)

        val plannings = solution.plannings
        val p = plannings(0)

        val workers = p.workerAssignments.head.workers
        workers should have size 3
      }

      search.solve()
    }

    it("Should be unsolvable") {
      an [NoSolutionException] should be thrownBy {
        getSearch(JsonParser.parse("data/test/additional-skills-impossible.json"), CPModelOptions().copy(allowPartial = false))
      }
    }

    it("Should be unsolvable - 2") {
      an [NoSolutionException] should be thrownBy {
        getSearch(JsonParser.parse("data/test/additional-skills-impossible2.json"), CPModelOptions().copy(allowPartial = false))
      }
    }

    it("Should be a partial solution") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-impossible.json"), CPModelOptions())
      search.onSolutionFound { solution =>
        checkPartial(solution)
      }
      search.solve()
    }

    it("Should be a partial solution - 2") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-impossible2.json"), CPModelOptions())
      search.onSolutionFound { solution =>
        checkPartial(solution)
      }
      search.solve()
    }
  }

  describe("Machine assignments") {
    it("Should return the correct machine assignments") {
      val search = getSearch(JsonParser.parse("data/test/machines-assignment.json"))

      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)

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
        checkValid(solution)
        checkNotPartial(solution)

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
        checkValid(solution)
        checkNotPartial(solution)

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

  describe("Worker/worker incompatibilities") {

    it("Should take worker/worker incompatibilities into account") {
      val search = getSearch(JsonParser.parse("data/test/Iww.json"))

      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)

        val plannings = solution.plannings
        plannings should have size 2

        val p0 = plannings(0)
        val p1 = plannings(1)

        val w1 = p0.workerAssignments.head.workers
        val w2 = p1.workerAssignments.head.workers

        if (w1.contains(0)) {
          w1 should not contain 1
          w1 should contain oneOf (2, 3)
          w2 should contain(1)
        }

        if (w1.contains(1)) {
          w1 should not contain 0
          w1 should contain oneOf (2, 3)
          w2 should contain(0)
        }

        if (w1.contains(2)) {
          w1 should not contain 3
          w1 should contain oneOf(0, 1)
          w2 should contain(3)
        }

        if (w1.contains(3)) {
          w1 should not contain 2
          w1 should contain oneOf(0, 1)
          w2 should contain(2)
        }
      }

      search.solve()
    }
  }

  describe("Worker/client incompatibilities") {

    it("Should take worker/client incompatibilities into account") {
      val search = getSearch(JsonParser.parse("data/test/Iwc.json"))

      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)

        val plannings = solution.plannings
        plannings should have size 2

        val p0 = plannings(0)
        val p1 = plannings(1)

        val w1 = p0.workerAssignments.head.workers
        val w2 = p1.workerAssignments.head.workers

        w1 should contain only (1, 2)
        w2 should contain only (0, 3)
      }

      search.solve()
    }
  }

  describe("Working requirements") {
    it("Should take into account working requirements") {
      val search = getSearch(JsonParser.parse("data/test/working-requirements.json"))
      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)
      }

      search.solve()

      val violations = search.lastSolution.violations
      violations.size should equal(1)
      violations.head should matchPattern { case WorkingRequirementViolation(_, _, _, v) => }
      inside(violations.head) {
        case WorkingRequirementViolation(_, _, _, v) =>
          v should equal(2)
      }
    }

    it("Should take into account working requirements (max)") {
      val search = getSearch(JsonParser.parse("data/test/working-requirements-max.json"))
      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)
      }

      search.solve()

      val violations = search.lastSolution.violations
      violations.size should equal(1)
      violations.head should matchPattern { case WorkingRequirementViolation(_, _, _, v) => }
      inside(violations.head) {
        case WorkingRequirementViolation(_, _, _, v) =>
          v should equal(5)
      }
    }
  }

  describe("Not enough workers") {

    val problem = JsonParser.parse("data/test/not-enough-workers.json")

    it("Should have the sentinel values") {
      val search = getSearch(problem)

      search.onSolutionFound { solution =>
        checkValid(solution)
        checkPartial(solution)


        val violations = solution.violations.filter { violation => violation match {
          case WorkerViolation(_, _, _) => true
          case _ => false
        }}

        violations.size should equal(2)
      }

      search.solve()
    }

    it("Should throw NoSolutionException (allowPartial = false)") {
      an [NoSolutionException] should be thrownBy {
        getSearch(problem, CPModelOptions().copy(allowPartial = false))
      }
    }
  }
}
