package village1.modeling.mip

import org.scalatest.Inside._
import village1.json.JsonParser
import village1.modeling._
import village1.modeling.violations.{WorkerViolation, WorkingRequirementViolation}
import village1.search.mip.{MIPSearch, MipSolverResult}

class VillageOneMIPModelSpec extends CommonSpec {


  private def getSearch(problem: Problem, options: MipModelOptions = MipModelOptions()): MIPSearch = {
    new MIPSearch(problem, options)
  }

  private def solve(search: MIPSearch): MipSolverResult = {
    search.solve(silent = true)
  }


  describe("Additional skills for demands") {
    it("Should return the correct workers assigned with additional skills") {
      val name = "additional-skills.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }

    it("Should return the correct workers assigned with additional skills (with special Max value)") {
      val name = "additional-skills-value.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }

    it("Should return the correct workers assigned with additional skills (with special Min value)") {
      val name = "additional-skills-value-2.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }


    it("Should be a partial solution") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-impossible.json"))
      search.onSolutionFound { solution =>
        checkPartial(solution)
      }

      solve(search)
    }

    it("Should be a partial solution - 2") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-impossible2.json"))
      search.onSolutionFound { solution =>
        println(solution)
        checkPartial(solution)

      }
      solve(search)
    }


    it("Should be unsolvable") {
      val search = getSearch(JsonParser.parse("data/test/additional-skills-impossible.json"), MipModelOptions().copy(allowPartial = false))
      val result = solve(search)
      result.solution should equal(null)
      search.lastSolution should equal(null)
    }

    it("Should be unsolvable - 2") {
      an [UnsolvableException] should be thrownBy {
        getSearch(JsonParser.parse("data/test/additional-skills-impossible2.json"), MipModelOptions().copy(allowPartial = false))
      }
    }
  }

  describe("Machine assignments") {
    it("Should return the correct machine assignments") {
      val name = "machines-assignment.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }
  }

  describe("Location assignments") {
    it("Should have the same location assigned") {
      val name = "locations-assignment.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }

    it("Should have different location assigned") {
      val name = "locations-assignment2.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }
  }


  describe("Worker/worker incompatibilities") {

    it("Should take worker/worker incompatibilities into account") {
      val name = "Iww.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }
  }

  describe("Worker/client incompatibilities") {

    it("Should take worker/client incompatibilities into account") {
      val name ="Iwc.json"
      val search = getSearch(parse(name))
      checkSolution(name)(search)
      solve(search)
    }
  }


  describe("Working requirements") {
    it("Should take into account working requirements (min)") {
      val search = getSearch(JsonParser.parse("data/test/working-requirements.json"), MipModelOptions().copy(allowPartial = false))
      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)
      }

      solve(search)

      val violations = search.lastSolution.violations
      violations.size should equal(1)
      violations.head should matchPattern { case WorkingRequirementViolation(_, _, _, v) => }
      inside(violations.head) {
        case WorkingRequirementViolation(_, _, _, v) =>
          v should equal(2)
      }
    }

    it("Should take into account working requirements (max)") {
      val search = getSearch(JsonParser.parse("data/test/working-requirements-max.json"), MipModelOptions().copy(allowPartial = false))
      search.onSolutionFound { solution =>
        checkValid(solution)
        checkNotPartial(solution)
      }

      solve(search)

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

//    it("Should throw NoSolutionException (allowPartial = false)") {
//      an [NoSolutionException] should be thrownBy {
//        getSearch(problem, CPModelOptions().copy(allowPartial = false))
//      }
//    }
  }


}
