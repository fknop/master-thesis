package village1.search.mip

import gurobi.{GRB, GRBCallback}
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.modeling.mip.VillageOneMIPModel
import village1.modeling.{Constants, Solution}
import village1.search.Search

class SolutionListener(model: VillageOneMIPModel) extends GRBCallback with Search {

  type WorkerVariablesSolution = Array[Array[Array[Array[Double]]]]
  type SentinelVariablesSolution = Array[Array[Array[Double]]]
  type ZoneVariablesSolution = Array[Array[Double]]
  type MachineVariablesSolution = Array[Array[Double]]

  private var _solution: Solution = _

  def solution: Solution = _solution

  private def getWorkerValues: WorkerVariablesSolution = {
    val variables = model.workerVariables
    variables.map(
      _.map(
        _.map(
          _.map(v =>
            if (v == null) 0 else getSolution(v)
          )
        )
      )
    )
  }

  private def getSentinelValues: SentinelVariablesSolution = {
    val variables = model.sentinelVariables
    variables.map(
      _.map(
        _.map(v => if (v == null) 0.0 else getSolution(v))
      )
    )
  }

  private def getMachineValues: MachineVariablesSolution = {
    val variables = model.machineVariables
    variables.map(
      _.map(
        getSolution
      )
    )
  }

  private def getZoneValues: MachineVariablesSolution = {
    val variables = model.zoneVariables
    variables.map(
      _.map(
        getSolution
      )
    )
  }

  // Only call this once model is optimized
  private def createSolution(): Solution = {

    val workerValues = getWorkerValues
    val zoneValues = getZoneValues
    val machineValues = getMachineValues
    val sentinelValues = getSentinelValues

    var demandAssignments: Array[DemandAssignment] = Array()

    for (d <- model.Demands) {
      val demand = model.demands(d)
      var workerAssignments: Array[WorkerAssignment] = Array()

      for (t <- demand.periods) {
        var workers = Array[Int]()

        for (p <- demand.positions) {
          val sentinel = sentinelValues(t)(d)(p)
          if (sentinel == 1.0) {
            workers :+= Constants.SentinelWorker
          }
          else {
            for (w <- model.Workers) {
              val value = workerValues(t)(d)(p)(w)
              if (value == 1.0) {
                workers :+= w
              }
            }
          }
        }

        workerAssignments :+= WorkerAssignment(workers, t)
      }

      val locationAssignment = if (demand.possibleLocations.isEmpty) {
        None
      } else {
        var location = -1
        for (l <- model.Locations) {
          val value = zoneValues(l)(d)
          if (value == 1.0) {
            location = l
          }
        }

        if (location == -1) {
          throw new UnknownError("No location have been assigned to solution")
        }

        Some(location)
      }

      val machineAssignments =
        if (demand.machineNeeds.isEmpty) None
        else {
          var machines = Array.fill(demand.machineNeeds.length)(-1)
          var i = 0
          for (m <- model.Machines) {
            val value = machineValues(m)(d)
            if (value == 1.0) {
              machines(i) = m
              i += 1
            }
          }

          Some(machines)
        }

      demandAssignments :+= DemandAssignment(d, workerAssignments, machineAssignments, locationAssignment)
    }


    val objective = this.getDoubleInfo(GRB.CB_MIPSOL_OBJ)

    Solution(model.problem, demandAssignments, objective.toInt)
  }


  override def callback(): Unit = {
    if (where == GRB.CB_MIPSOL) {
      _solution = createSolution()
      emitSolution(solution)
    }
  }
}