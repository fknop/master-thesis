package village1.modeling

import village1.data.DemandAssignment

case class Solution(problem: Problem, plannings: Array[DemandAssignment], objective: Int = 0) {

  def valid: (Boolean, String) = {

    val workers = problem.workers
    val demands = problem.demands

    val workersAtTime: Array[Array[Int]] = Array.fill(problem.T)(Array[Int]())
    val locationsAtTime: Array[Array[Int]] = Array.fill(problem.T)(Array[Int]())
    val machinesAtTime: Array[Array[Int]] = Array.fill(problem.T)(Array[Int]())

    for (planning <- plannings) {
      val demand = demands(planning.demand)

      if (planning.workerAssignments.size != demand.periods.size) {
        return (false, "The number of timeslots is not satisfied")
      }

      planning.locationAssignment match {
        case Some(assignment) =>
          for (t <- demand.periods) {
            locationsAtTime(t) ++= Array(assignment)
          }

          if (!demand.possibleLocations.contains(assignment)) {
            return (false, s"Location $assignment is not a possible position for demand ${planning.demand}")
          }
        case None =>
      }

      planning.machineAssignments match {
        case Some(assignments) =>
          for (t <- demand.periods) {
            machinesAtTime(t) ++= assignments
          }

          var i = 0
          var used = Set[Int]()
          var satisfied = 0
          while (i < demand.machineNeeds.length) {
            val need = demand.machineNeeds(i)
            for (j <- assignments.indices if !used.contains(j)) {
              if (need.name == problem.machines(assignments(j)).name) {
                used += j
                satisfied += 1
              }
            }
            i += 1
          }

          if (satisfied != demand.machineNeeds.length) {
            return (false, "Machine assignment")
          }
        case None =>
      }


      for (assignment <- planning.workerAssignments) {
        val t = assignment.timeslot
        val w = assignment.workers

        if (w.length != demand.requiredWorkers) {
          return (false, "The number of required workers is not satisfied")
        }

        if (!demand.periods.contains(t)) {
          return (false, s"The demand does not contain timeslot $t")
        }

        if (!allDifferent(w)) {
          return (false, "The same worker works at two different positions")
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
              return (false, s"Worker $worker does not satisfy requirements($r) for demand ${demand.id} at time ${t} not satisfied")
            }
          }
        }

        val additional = demand.additionalSkills
        for (skill <- additional) {
          var satisfied = false
          for (worker <- w) {
            satisfied = satisfied || workers(worker).satisfySkill(skill)
          }

          if (!satisfied) {
            (false, s"Additional skill: ${skill.name} not satisfied")
          }
        }

      }
    }



    for (t <- 0 until problem.T) {
      if (!allDifferent(workersAtTime(t))) {
        return (false, s"!allDifferent(workersAtTime($t))")
      }

      if (!allDifferent(locationsAtTime(t))) {
        return (false, s"!allDifferent(locationsAtTime($t))")
      }

      if (!allDifferent(machinesAtTime(t))) {
        return (false, s"!allDifferent(machinesAtTime($t))")
      }
    }


    (true, "OK")
  }

  private[this] def allDifferent(array: Array[Int]): Boolean = {
    Set[Int](array: _*).size == array.length
  }
}

