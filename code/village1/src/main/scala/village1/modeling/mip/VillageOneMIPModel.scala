package village1.modeling.mip

import gurobi._
import village1.data.{DemandAssignment, WorkerAssignment, WorkingRequirement}
import village1.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, UnsolvableException, VillageOneModel}
import village1.search.cp.{VillageOneLNS, VillageOneSearch}
import village1.util.BenchmarkUtils.time
import village1.util.Utils
import village1.modeling.Constants._

import scala.util.Random



object VillageOneMIPModel {
  private val env: GRBEnv = new GRBEnv("mip.log")
  sys.ShutdownHookThread {
    env.dispose()
  }
}


case class MipModelOptions(symmetryBreaking: Boolean = true, allowPartial: Boolean = true, objective: Boolean = true)

class VillageOneMIPModel(problem: Problem, options: MipModelOptions = MipModelOptions(), v1model: Option[VillageOneModel] = None) extends VillageOneModel(problem, v1model) {

  def this(v1model: VillageOneModel) = this(v1model.problem, v1model = Some(v1model))
  def this(v1model: VillageOneModel, options: MipModelOptions) = this(v1model.problem, options, v1model = Some(v1model))

  type WorkerVariables = Array[Array[Array[Array[GRBVar]]]]
  type SentinelVariables = Array[Array[Array[GRBVar]]]
  type ZoneVariables = Array[Array[GRBVar]]
  type MachineVariables = Array[Array[GRBVar]]

  private val env = VillageOneMIPModel.env
  val model: GRBModel = new GRBModel(env)

  val sentinelVariables: SentinelVariables = createSentinelWorkers(model)
  val workerVariables: WorkerVariables = createWorkerVariables(model)
  val zoneVariables: ZoneVariables = createZoneVariables(model)
  val machineVariables: MachineVariables = createMachineVariables(model)

  initialize()


  private def createWorkerVariables (model: GRBModel): WorkerVariables = {

    Array.tabulate(T, D) { (t, d) =>
      Array.tabulate(demands(d).requiredWorkers, W)  {(p, w) =>

        val impossible = (!demands(d).periods.contains(t)) ||
          (!possibleWorkersForDemands(d)(t)(p).contains(w))

        if (impossible) null
        else model.addVar(0, 1, 0.0, GRB.BINARY, s"w[$t][$d][$p][$w]")
      }
    }
  }

  private def createSentinelWorkers (model: GRBModel): SentinelVariables = {

    Array.tabulate(T, D) { (t, d) =>
      Array.tabulate(demands(d).requiredWorkers)  {p =>
        if (demands(d).periods.contains(t))
          model.addVar(0, 1, 0.0, GRB.BINARY, s"sentinel[$t][$d][$p]")
        else
          null
      }
    }
  }

  private def createZoneVariables(model: GRBModel): ZoneVariables = {
    Array.tabulate(L, D)((l, d) => {
      model.addVar(0, 1, 0.0, GRB.BINARY, s"z[$l][$d]")
    })
  }

  private def createMachineVariables(model: GRBModel): MachineVariables = {
    Array.tabulate(M, D)((m, d) => {
      model.addVar(0, 1, 0.0, GRB.BINARY, s"m[$m][$d]")
    })
  }


  private def removeWorkerSymmetries (): Unit = {
    val expression = new GRBLinExpr()
    for (d <- Demands) {
      for (t <- demands(d).periods) {

        val all = possibleWorkersForDemands(d)(t).foldLeft(Set[Int]())( (acc, s) => acc.union(s))
        if (all.size < demands(d).requiredWorkers) {
          return
        }

        val symmetries = Utils.groupByEquality(possibleWorkersForDemands(d)(t))
        if (symmetries.nonEmpty) {
          val possibleWithoutSymmetries = Utils.removeSymmetries(possibleWorkersForDemands(d)(t), symmetries)
          for (symmetry <- symmetries) {
            for (p <- symmetry) {
              val possible = possibleWithoutSymmetries(p)
              for (value <- possible) {
                expression.addTerm(1, workerVariables(t)(d)(p)(value))
//                variables(t)(d)(p)(value).set(GRB.DoubleAttr.UB, 0)
              }
            }
          }
        }
      }
    }

    model.addConstr(expression, GRB.EQUAL, 0, "symmetries")
  }
//
//  // TODO: remove constraints and add to initialization
//  def removeImpossibleValues (model: GRBModel, variables: WorkerVariables): Unit = {
//    val expression = new GRBLinExpr()
//    for (t <- Periods; d <- Demands; w <- Workers) {
//
//      val impossible = (!demands(d).periods.contains(t)) ||
//                       (!availableWorkers(d).contains(t)) ||
//                       (!availableWorkers(d)(t).contains(w))
//
//      if (impossible) {
//        for (p <- demands(d).positions) {
//         // variables(t)(d)(p)(w).set(GRB.DoubleAttr.UB, 0.0)
//          expression.addTerm(1, variables(t)(d)(p)(w))
////          model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"imp[$t][$d][$p][$w]")
//        }
//      }
//    }
//
//    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
//      val workers = possibleWorkersForDemands(d)(t)(p)
//      for (w <- allWorkers.diff(workers)) {
////      variables(t)(d)(p)(w).set(GRB.DoubleAttr.UB, 0)
  //        expression.addTerm(1, variables(t)(d)(p)(w))
  ////        model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"requiredSkill[$t][$d][$p][$w]")
  //      }
  //    }
  //
  //    model.addConstr(expression, GRB.EQUAL, 0, s"impossibleValues")
  //  }



  private def allDifferentWorkers (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- Periods; w <- Workers) {
      val expression = new GRBLinExpr()
      for (d <- Demands if demands(d).periods.contains(t); p <- demands(d).positions) {
        if (variables(t)(d)(p)(w) != null) {
          expression.addTerm(1, variables(t)(d)(p)(w))
        }
      }

      model.addConstr(expression, GRB.LESS_EQUAL, 1, s"c1[$t][$w]")
    }
  }

  private def workerNumberSatisfied (model: GRBModel, variables: WorkerVariables, sentinels: SentinelVariables): Unit = {
    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
      val expression = new GRBLinExpr()
      for (w <- possibleWorkersForDemands(d)(t)(p)) {
        expression.addTerm(1, variables(t)(d)(p)(w))
      }

      if (options.allowPartial) {
        expression.addTerm(1, sentinels(t)(d)(p))
      }

      model.addConstr(expression, GRB.EQUAL, 1, s"c2[$t][$d][$p]")
    }
  }

  private def applyWorkingRequirements (model: GRBModel, variables: WorkerVariables): GRBLinExpr = {
    val violations = new GRBLinExpr()
    for (r <- problem.workingRequirements) {
      val w = r.worker
      val occurrences = new GRBLinExpr()
      for (d <- Demands; t <- demands(d).periods if workers(w).available(t); p <- demands(d).positions) {
        val variable = variables(t)(d)(p)(w)
        if (variable != null) {
          occurrences.addTerm(1, variable)
        }
      }

      val sum = model.addVar(0, workers(w).availabilities.size, 0, GRB.INTEGER, s"occurrences[$w]")
      occurrences.addTerm(-1, sum)
      model.addConstr(occurrences, GRB.EQUAL, 0, "occurrencesSum[$w]")

      r.min match {
        case Some(min) =>
          val minimum = model.addVar(0, workers(w).availabilities.size, 0, GRB.INTEGER, s"requirementMinimum[$w]")
          val difference = model.addVar(-workers(w).availabilities.size, workers(w).availabilities.size, 0, GRB.INTEGER, s"requirementMinDiff[$w]")
          val expression = new GRBLinExpr()
          expression.addConstant(min)
          expression.multAdd(-1, occurrences)
          expression.addTerm(-1, difference)
          model.addConstr(expression, GRB.EQUAL, 0, s"requirementMinDiffCtr[$w]")
          model.addGenConstrMin(minimum, Array(difference), 0, s"requirementMinCtr[$w]")
          violations.addTerm(1, minimum)
        case None =>
      }

      r.max match {
        case Some(max) =>
          val maximum = model.addVar(0, workers(w).availabilities.size, 0, GRB.INTEGER, s"requirementMaximum[$w]")
          val difference = model.addVar(-workers(w).availabilities.size, workers(w).availabilities.size, 0, GRB.INTEGER, s"requirementMaxDiff[$w]")
          val expression = new GRBLinExpr()
          expression.addConstant(-max)
          expression.add(occurrences)
          expression.addTerm(-1, difference)
          model.addConstr(expression, GRB.EQUAL, 0, s"requirementMaxDiffCtr[$w]")
          model.addGenConstrMax(maximum, Array(difference), 0, s"requirementMaxCtr[$w]")
          violations.addTerm(1, maximum)
        case None =>
      }
    }

    violations
  }
//
//  private def applySentinels(model: GRBModel, workerVariables: WorkerVariables, sentinels: SentinelVariables): Unit = {
//    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
//      val expression = new GRBLinExpr()
//      for (w <- Workers) {
//        val variable = workerVariables(t)(d)(p)(w)
//        if (variable != null) {
//          expression.addTerm(1, variable)
//        }
//      }
//
//      val tmp = model.addVar(0, 1, 0, GRB.BINARY, s"sentinelTmp[$d][$t][$p]")
//      model.addConstr(expression, GRB.EQUAL, tmp, s"sentinelTmpCtr[$d][$t][$p]")
//      model.addGenConstrIndicator(sentinels(t)(d)(p), 1, expression, GRB.EQUAL, 0, s"sentinelCtr[$d][$t][$p]")
//    }
//  }

  private def satisfyWorkerWorkerIncompatibilities (model: GRBModel, variables: WorkerVariables): Unit = {
    val incompatibilities = problem.workerWorkerIncompatibilities
    for (incompatibility <- incompatibilities) {
      val w0 = incompatibility(0)
      val w1 = incompatibility(1)

      for (t <- Periods; d <- Demands if demands(d).periods.contains(t)) {
        val expression = new GRBLinExpr()
        for (p <- demands(d).positions) {
          expression.addTerm(1.0, variables(t)(d)(p)(w0))
          expression.addTerm(1.0, variables(t)(d)(p)(w1))
        }
        model.addConstr(expression, GRB.LESS_EQUAL, 1, s"Iww[$w0][$w1]")
      }

    }
  }


  private def satisfyWorkerClientIncompatibilities (model: GRBModel, variables: WorkerVariables): Unit = {
    val incompatibilities = problem.workerClientIncompatibilities
    for (incompatibility <- incompatibilities) {
      val iw = incompatibility(0)
      val ic = incompatibility(1)


      for (d <- Demands; t <- demands(d).periods) {
        if (demands(d).client == ic) {
          for (p <- demands(d).positions) {
            model.addConstr(variables(t)(d)(p)(iw), GRB.EQUAL, 0, s"iwc[$t][$d][$p][$iw]")
          }
        }
      }
    }
  }

  private def satisfyAdditionalSkills(model: GRBModel, variables: WorkerVariables, sentinels: SentinelVariables): Unit = {
    for (d <- Demands) {
      val demand = demands(d)
      for (t <- demand.periods) {

        // Take only all the possible workers for that demand
        val possibleWorkersForDemand = possibleWorkersForDemands(d)(t).reduce((a, b) => a.union(b))

        for (skill <- demand.additionalSkills) {
          val name = skill.name

          val possibleWorkers = possibleWorkersForDemand.intersect(workersWithSkills(name))
            .filter(workers(_).satisfySkill(skill))
            .union(if (options.allowPartial) Set(SentinelWorker) else Set())

          if (possibleWorkers.isEmpty) {
            throw new UnsolvableException(s"No workers with skill $name")
          }

          val expression = new GRBLinExpr()
          for (w <- possibleWorkers) {
            for (p <- demand.positions) {

              if (w == SentinelWorker) {
                expression.addTerm(1, sentinels(t)(d)(p))
              }
              else {
                expression.addTerm(1, workerVariables(t)(d)(p)(w))
              }

            }
          }

          model.addConstr(expression, GRB.GREATER_EQUAL, 1, s"skill+[$d][$t]")
        }
      }
    }
  }

  private def removeImpossibleZones(model: GRBModel, variables: ZoneVariables): Unit = {
    val expression = new GRBLinExpr()

    for (l <- Locations; d <- Demands) {
      if (!demands(d).possibleLocations.contains(l)) {
        expression.addTerm(1, variables(l)(d))
      }
    }

    model.addConstr(expression, GRB.EQUAL, 0, "z1")
  }

  private def applyAllDifferentLocations(model: GRBModel, variables: ZoneVariables): Unit = {
    for (d <- Demands) {
      val overlappingDemands = overlappingSets(d)
      if (overlappingDemands.nonEmpty) {
        for (o <- overlappingDemands; l <- Locations) {
          val expression = new GRBLinExpr()
          expression.addTerm(1, variables(l)(o))
          expression.addTerm(1, variables(l)(d))
          model.addConstr(expression, GRB.LESS_EQUAL, 1, s"z2[$d][$o]")
        }
      }
    }
  }

  private def satisfyDemandLocation(model: GRBModel, variables: ZoneVariables): Unit = {
    for (d <- Demands if demands(d).possibleLocations.nonEmpty) {
      val expression = new GRBLinExpr()
      for (l <- demands(d).possibleLocations) {
        expression.addTerm(1.0, variables(l)(d))
      }

      model.addConstr(expression, GRB.EQUAL, 1, s"z3[$d]")
    }
  }

  private def removeImpossibleMachines(model: GRBModel, variables: MachineVariables): Unit = {
    val expression = new GRBLinExpr()

    for (d <- Demands) {
      val possible: Set[Int] =
        if (demands(d).machineNeeds.nonEmpty)
          demands(d).machineNeeds.map(machine => possibleMachines(machine.name)).reduce(_.union(_))
        else
          Set()

      for (m <- Machines) {
        if (!possible.contains(m)) {
          expression.addTerm(1, variables(m)(d))
        }
      }
    }

    model.addConstr(expression, GRB.EQUAL, 0, "m1")
  }

  private def applyAllDifferentMachines(model: GRBModel, variables: MachineVariables): Unit = {
    for (d <- Demands) {
      val overlappingDemands = overlappingSets(d)
      if (overlappingDemands.nonEmpty) {
        for (o <- overlappingDemands; m <- Machines) {
          val expression = new GRBLinExpr()
          expression.addTerm(1, variables(m)(o))
          expression.addTerm(1, variables(m)(d))
          model.addConstr(expression, GRB.LESS_EQUAL, 1, s"m2[$d][$o]")
        }
      }
    }
  }

  private def satisfyMachinesDemand(model: GRBModel, variables: MachineVariables): Unit = {
    for (d <- Demands if demands(d).machineNeeds.nonEmpty) {
      var needs: Map[String, Int] = Map()
      for (machine <- demands(d).machineNeeds) {
        needs = needs.updated(machine.name, needs.getOrElse(machine.name, 0) + 1)
      }

      for ((name, occurrence) <- needs) {
        val expression = new GRBLinExpr()
        val possible = possibleMachines(name)
        for (m <- possible) {
          expression.addTerm(1.0, variables(m)(d))
        }
        model.addConstr(expression, GRB.EQUAL, occurrence, s"z3[$d]")
      }
    }
  }


  // TODO: this works for now for simple models - check for larger ones
  /**
    * Minimize shift change between workers at one position
    */
  private def minimizeShiftChange (model: GRBModel, variables: WorkerVariables, sentinels: SentinelVariables): GRBLinExpr = {
    val expression = new GRBLinExpr()
    for (d <- Demands; p <- demands(d).positions) {

      for (w <- Workers) {
        // All the variables for this worker at demand d and position p
        val vars = demands(d).periods.map(variables(_)(d)(p)(w)).filterNot(_ == null).toArray

        // Binary variable: is the worker working for that shift at at least one time period ?
        val isWorking = model.addVar(0, 1.0, 0, GRB.BINARY, s"isWorker[$d][$p][$w]")
        model.addGenConstrOr(isWorking, vars, s"isWorkingConstr[$d][$p][$w]")

        expression.addTerm(1, isWorking)
      }

      // TODO: verify this
      val hasSentinel = model.addVar(0, 1, 0, GRB.BINARY, s"hasSentinal[$d][$p]")
      model.addGenConstrOr(
        hasSentinel,
        demands(d).periods.map(sentinels(_)(d)(p)).filterNot(_ == null).toArray,
        s"hasSentinalConstr[$d][$p]"
      )

      expression.addTerm(1, hasSentinel)
    }

    // Minimize the number of working workers at each position
    expression
  }

  def minimizeSentinelWorkers (model: GRBModel, variables: SentinelVariables): GRBLinExpr = {
    val expression = new GRBLinExpr()
    for (d <- Demands; p <- demands(d).positions; t <- demands(d).periods) {
      expression.addTerm(1, variables(t)(d)(p))
    }

    expression
  }

  private def minimizeObjectives(expressions: Array[GRBLinExpr]): Unit = {
    val objective = new GRBLinExpr()
    expressions.foreach(objective.add)
    model.setObjective(objective, GRB.MINIMIZE)
  }

  def setInitialSolution(solution: Solution, probability: Double = 0.5): Unit = {
    val rand = new Random(0)
    for (demandAssignment <- solution.plannings) {
      val d = demandAssignment.demand
      val workerAssignments = demandAssignment.workerAssignments
      for (assignment <- workerAssignments) {
        val t = assignment.timeslot
        val workers = assignment.workers
        for (i <- workers.indices) {
          if (rand.nextDouble() <= probability) {
            workerVariables(t)(d)(i)(workers(i)).set(GRB.DoubleAttr.Start, 1.0)
          }
        }
      }
    }
  }

  private def applyObjectives (): Unit = {
    val objective = Array(
      minimizeShiftChange(model, workerVariables, sentinelVariables),
      minimizeSentinelWorkers(model, sentinelVariables),
      applyWorkingRequirements(model, workerVariables)
    )

    minimizeObjectives(objective)
  }

  private def applyConstraints (): Unit = {
    allDifferentWorkers(model, workerVariables)
    workerNumberSatisfied(model, workerVariables, sentinelVariables)
    satisfyWorkerWorkerIncompatibilities(model, workerVariables)
    satisfyWorkerClientIncompatibilities(model, workerVariables)
    satisfyAdditionalSkills(model, workerVariables, sentinelVariables)

    removeImpossibleZones(model, zoneVariables)
    applyAllDifferentLocations(model, zoneVariables)
    satisfyDemandLocation(model, zoneVariables)

    removeImpossibleMachines(model, machineVariables)
    applyAllDifferentMachines(model, machineVariables)
    satisfyMachinesDemand(model, machineVariables)
  }




  private def initialize(): Unit = {

    if (options.symmetryBreaking) {
      removeWorkerSymmetries()
    }

    applyConstraints()

    if (options.objective) {
      applyObjectives()
    }
  }
}

