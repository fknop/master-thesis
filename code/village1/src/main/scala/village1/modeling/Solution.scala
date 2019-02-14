package village1.modeling

import village1.data.{DemandAssignment}

case class Solution(problem: Problem, plannings: Array[DemandAssignment]) {

  // TODO: implement machines / locations logic
  // TODO: implement additional skills logic
  // - Check if all workers for at each time period are different
  // - Check if all workers satisfy demand requirements
  def valid(): Boolean = {

    val workers = problem.workers
    val demands = problem.demands

    val workersAtTime: Array[Array[Int]] = Array.fill(problem.T)(Array[Int]())

    for (planning <- plannings) {
      val demand = demands(planning.demand)

      if (planning.workerAssignments.size != demand.periods.size) {
        return false
      }


      for (assignment <- planning.workerAssignments) {
        val t = assignment.timeslot
        val w = assignment.workers

        if (w.length != demand.requiredWorkers) {
          return false
        }

        if (!demand.periods.contains(t)) {
          return false
        }

        if (!allDifferent(w)) {
          return false
        }

        workersAtTime(t) ++= w

        // TODO: restrictions
        val requirements = demand.requirements

        // TODO: might be false if solution return workers in another order
        for (r <- requirements.indices) {
          val skills = requirements(r).skills
          if (skills.length > 0) {
            val worker = workers(w(r))

            if (!worker.satisfySkills(skills)) {
              return false
            }
          }
        }
      }
    }



    for (t <- 0 until problem.T) {
      if (!allDifferent(workersAtTime(t))) {
        return false
      }
    }


    true
  }

  private[this] def allDifferent(array: Array[Int]): Boolean = {
    Set[Int](array: _*).size == array.length
  }
}

