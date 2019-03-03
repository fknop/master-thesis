package village1.modeling.cp

import oscar.cp._
import oscar.cp.constraints.AtLeastNValue
import oscar.cp.core.CPPropagStrength
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.util.Utilities


class VillageOneCPModel(problem: Problem, model: Option[VillageOneModel] = None) extends VillageOneModel(problem, model) with CPModel {

  def this(model: VillageOneModel) = this(model.problem, Some(model))

  type WorkerVariables = Array[Array[Array[CPIntVar]]]

  // We make a guess (for now) that machines and locations will be the same for the entire duration of the demand
  // and that overlapping demands will not be able to have the same locations.
  type MachineVariables = Array[Array[CPIntVar]]
  type LocationVariables = Array[CPIntVar]


  private [this] val EMPTY_INT_VAR_ARRAY = Array.empty[CPIntVar]

  private[this] val overlappingSets = Utilities.overlappingSets(problem.demands)



  val workerVariables: WorkerVariables = generateWorkerVariables()
  val machineVariables: MachineVariables = generateMachineVariables()
  val locationVariables: LocationVariables = generateLocationVariables()

  val shiftNWorkers: Array[Array[CPIntVar]] = Array.tabulate(D)(d => Array.tabulate(demands(d).requiredWorkers)(_ => CPIntVar(1 to demands(d).periods.size)))
  val objective: CPIntVar = sum(shiftNWorkers.flatten)


  initialize()



  // Workers constraints


  def initialize (): Unit = {
    removeWorkerSymmetries()
    applyAllDifferentWorkers()
    applyWorkerWorkerIncompatibilities()
    applyWorkerClientIncompatibilities()
    applyAdditionalSkills()

    // Locations constraints
    applyAllDifferentLocations()

    // Machine constraints
    applyAllDifferentMachines()

    computeShiftNWorkers()
  }



  // Methods definitions


  private def generateWorkerVariables (): WorkerVariables = {
    Array.tabulate(T, D)((t, d) => {
      val demand = demands(d)

      if (demand.hasPeriod(t)) {
        Array.tabulate(demand.requiredWorkers)(i => CPIntVar(possibleWorkersForDemands(d)(t)(i)))
      }
      else {
        EMPTY_INT_VAR_ARRAY
      }
    })
  }


  private def generateLocationVariables (): LocationVariables = {
    Array.tabulate(D)(d => {
      if (demands(d).possibleLocations.isEmpty) null
      // Filter locations that might be out of range
      // TODO: throw a warning/error if location is out of range.
      else CPIntVar(demands(d).possibleLocations.filter(l => 0 <= l && l < L))
    })
  }

  private def generateMachineVariables (): MachineVariables = {
    Array.tabulate(D)(d => {
      val demand = demands(d)
      Array.tabulate(demand.machineNeeds.length)(m => {
        val possibleValues = possibleMachines(demand.machineNeeds(m).name)
        CPIntVar(possibleValues)
      })
    })
  }

  private def removeWorkerSymmetries (): Unit = {
    for (d <- Demands) {
      for (t <- demands(d).periods) {
        val symmetries = Utilities.groupByEquality(possibleWorkersForDemands(d)(t))
        if (symmetries.nonEmpty) {
          val possibleWithoutSymmetries = Utilities.removeSymmetries(possibleWorkersForDemands(d)(t), symmetries)
          for (symmetry <- symmetries) {
            for (p <- symmetry) {
              val possible = possibleWithoutSymmetries(p)
              for (value <- possible) {
                workerVariables(t)(d)(p).removeValue(value)
              }
            }
          }
        }
      }
    }
  }
  // All workers for a given time must be different
  private def applyAllDifferentWorkers (): Unit = {
    for (period <- Periods) {
      val workersForPeriod = workerVariables(period).flatten

      if (!workersForPeriod.isEmpty) {
        add(allDifferent(workersForPeriod))
      }
    }
  }

  // All zones for a given time must be different
  private def applyAllDifferentLocations(): Unit = {
    for (d <- Demands if locationVariables(d) != null) {
      val overlappingDemands = overlappingSets(d)
      val locations = (overlappingDemands + d).map(locationVariables(_)).filter(_ != null)
      add(allDifferent(locations))
    }


    /**
    for (period <- Periods) {
      val zonesForPeriod = locationVariables(period).filter(_ != null)
      if (zonesForPeriod.length >= 2) {
        add(allDifferent(zonesForPeriod))
      }
    }
    **/
  }


  private def applyAllDifferentMachines(): Unit = {
    for (d <- Demands) {
      val overlappingDemands = overlappingSets(d)
      val machines = (overlappingDemands + d).flatMap(machineVariables(_))
      if (machines.nonEmpty) {
        add(allDifferent(machines))
      }
    }
  }


  private def applyWorkerWorkerIncompatibilities(): Unit = {
    val wwIncompatibilities = problem.workerWorkerIncompatibilities ++ problem.workerWorkerIncompatibilities.map(_.reverse)

    // Workers with incompatibilities cannot work together
    for (period <- Periods; demand <- Demands) {
      val demandVar = workerVariables(period)(demand)
      if (demandVar.length >= 2) {
        val permutations = Utilities.generatePermutationsOfTwo(demandVar.length)
        for ((i, j) <- permutations) {
          add(negativeTable(Array(demandVar(i), demandVar(j)), wwIncompatibilities))
        }
      }
    }
  }

  private def applyWorkerClientIncompatibilities (): Unit = {
    val wcIncompatibilities = problem.workerClientIncompatibilities

    // For each incompatibility between a client and a worker
    // Remove the worker from the demands where the worker cannot work with that client
    for (incompatibility <- wcIncompatibilities) {
      val workerId = incompatibility(0)
      val client = incompatibility(1)
      for (d <- Demands) {
        val demand = demands(d)
        if (demand.client == client) {
          for (period <- demand.periods) {
            val demandWorkers: Array[CPIntVar] = workerVariables(period)(d)
            for (worker <- demandWorkers) {
              add(worker !== workerId)
            }
          }
        }
      }
    }
  }



  /**
    * Apply a constraint on additional skills that any workers in the team
    * can have, the same worker can have multiple additional skills, as long as one
    * worker in the team have the skill, the constraint is satisfied
    *
    * For now, a gcc constraint is used. But a custom constraint could also be used.
    */
  private def applyAdditionalSkills (): Unit = {
    for (d <- Demands) {
      val demand = demands(d)
      for (t <- demand.periods) {
        val workersForDemand = workerVariables(t)(d)

        for (skill <- demand.additionalSkills) {
          val name = skill.name

          // Take only all the possible workers for that demand
          val possibleWorkers = possibleWorkersForDemands(d)(t).reduce((a, b) => a.union(b)).intersect(workersWithSkills(name))

          if (possibleWorkers.isEmpty) {
            throw new NoSolutionException(s"No workers with skill $name")
          }

          val valueOccurrences = possibleWorkers.map(w => (w, CPBoolVar()))
          val occurrences = valueOccurrences.map(_._2)

          // At least one worker has the skill
          add(sum(occurrences) >= 1)
          add(gcc(workersForDemand, valueOccurrences))
        }
      }
    }
  }

  /**
    * A worker should work on the same demand as time goes on
    * Count the number of different workers assigned to a shift
    * TODO: implementation (check if this is the best way to do this)
    */
  private def computeShiftNWorkers (): Unit = {
    for (d <- Demands) {
      val demand = demands(d)
      for (w <- 0 until demand.requiredWorkers) {
        val workersForDemand = demand.periods.map(t => workerVariables(t)(d)(w)).toArray
        if (workersForDemand.length > 1) {
          add(new AtLeastNValue(workersForDemand, shiftNWorkers(d)(w)), CPPropagStrength.Weak)
        }
      }
    }
  }



  def createSolution(): Solution = {
    val demandAssignments: Array[DemandAssignment] = Array.fill(D)(null)

    var d = 0
    while (d < D) {
      val demand = demands(d)
      val slots = demand.periods

      val machineValues = machineVariables(d)
      val locationValue = locationVariables(d)

      val machineAssignments: Option[Array[Int]] =
        if (machineValues.nonEmpty)
          Some(machineValues.map(_.value))
        else
          None

      val locationAssignment =
        if (locationValue != null) Some(locationValue.value)
        else None

      val workerAssignments: Array[WorkerAssignment] = Array.fill(slots.size)(null)

      var i = 0

      for (t <- slots) {
        val workerValues = workerVariables(t)(d)
        val workers = Array.fill(workerValues.length)(0)
        var w = 0
        while (w < workers.length) {
          workers(w) = workerValues(w).value
          w += 1
        }

        workerAssignments(i) = WorkerAssignment(workers, t)
        i += 1
      }

      demandAssignments(d) = DemandAssignment(d, workerAssignments, machineAssignments, locationAssignment)
      d += 1
    }


    Solution(problem, demandAssignments, objective.value)
  }


}
