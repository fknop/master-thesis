package village1.modeling.cp

import org.scalatest._
import oscar.cp.core.NoSolutionException
import village1.search.cp.VillageOneSearch

class VillageOneCPModelSpec extends FunSpec with Matchers {
  describe("Additional skills for demands") {
    it("Should return the correct workers assigned with additional skills") {
      val search = new VillageOneSearch("data/test/additional-skills.json")

      search.onSolutionFound { solution =>
        solution.valid() should be (true, "OK")
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

    it("Should be unsolvable") {
      an [NoSolutionException] should be thrownBy {
        new VillageOneSearch("data/test/additional-skills-impossible.json")
      }
    }
  }


}
