package village1.modeling

import org.scalatest.{FunSpec, Matchers}
import village1.json.JsonParser
import village1.search.SolutionEmitter

abstract class CommonSpec extends FunSpec with Matchers {
  protected def checkValid (solution: Solution): Unit = {
    solution.valid should matchPattern { case ValidSolution => }
  }

  protected def checkNotPartial(solution: Solution): Unit = {
    solution.partial should equal(false)
  }

  protected def checkPartial(solution: Solution): Unit = {
    solution.partial should equal(true)
  }

  protected def parse(name: String): Problem = {
    JsonParser.parse(s"data/test/$name")
  }


  protected val checkSolution: Map[String, SolutionEmitter => Unit] = Map(
    "additional-skills.json" -> additionalSkills_1,
    "additional-skills-value.json" -> additionalSkills_2,
    "additional-skills-value-2.json" -> additionalSkills_3,
    "Iww.json" -> iww_1,
    "Iwc.json" -> iwc_1,
    "machines-assignment.json" -> machine_1,
    "locations-assignment.json" -> location_1,
    "locations-assignment2.json" -> location_2
  )

  private def machine_1(search: SolutionEmitter): Unit = {
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
  }

  private def location_1(search: SolutionEmitter): Unit = {
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
  }

  private def location_2(search: SolutionEmitter): Unit = {
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
  }


  private def additionalSkills_1(search: SolutionEmitter): Unit = {
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
  }

  private def additionalSkills_2(search: SolutionEmitter): Unit = {
    search.onSolutionFound { solution =>
      checkValid(solution)
      checkNotPartial(solution)

      val plannings = solution.plannings
      val p = plannings(0)

      val workers = p.workerAssignments.head.workers
      workers should have size 3
    }
  }

  private def additionalSkills_3(search: SolutionEmitter): Unit = {
    search.onSolutionFound { solution =>
      checkValid(solution)
      checkNotPartial(solution)

      val plannings = solution.plannings
      val p = plannings(0)

      val workers = p.workerAssignments.head.workers
      workers should have size 3
    }
  }

  private def iww_1(search: SolutionEmitter): Unit = {
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
  }


  private def iwc_1(search: SolutionEmitter): Unit = {
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
  }



  }
