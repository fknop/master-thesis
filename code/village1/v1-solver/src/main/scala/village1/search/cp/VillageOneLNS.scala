package village1.search.cp

import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.json.JsonSerializer
import village1.modeling.cp.{CPModelOptions, VillageOneCPModel}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.search.cp.heuristic.{Heuristic, MostAvailableHeuristic}
import village1.search.cp.relaxations.PropagationGuidedRelaxation
import village1.search.{Search, SearchResult}
import village1.util.SysUtils.time

import scala.util.Random



case class LNSOptions(repeat: Int = Int.MaxValue, limit: Int = 2500, bestWorkingViolations: Int = 0)

class VillageOneLNS(problem: Problem, options: CPModelOptions = CPModelOptions(), base: Option[VillageOneModel] = None)
      extends VillageOneCPModel(problem, options, base)
      with Search[LNSOptions] {

  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))
  def this(base: VillageOneModel, options: CPModelOptions) = this(base.problem, options, Some(base))

  val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
  val flatMachines: Array[CPIntVar] = machineVariables.flatten
  val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)

  private val toFlat: Array[Map[Int, Array[Int]]] = buildToFlat()

  solver.addDecisionVariables(flatWorkers)
  solver.addDecisionVariables(flatMachines)
  solver.addDecisionVariables(flatLocations)

  var currentSolution: CPIntSol = _
  var bestObjective: Int = Int.MaxValue
  var bestObjective1: Int = Int.MaxValue
  var bestObjective2: Int = Int.MaxValue
  var bestObjective3: Int = Int.MaxValue

//  private var relaxation: Relaxation = new RandomRelaxation(this)
  private val heuristic: Heuristic = new MostAvailableHeuristic(this, flatWorkers, workerVariables)
  private val relaxation = new PropagationGuidedRelaxation()

  search {
    var branching = heuristic.branching

    if (flatMachines.nonEmpty) {
      branching ++= binaryFirstFail(flatMachines)
    }

    if (flatLocations.nonEmpty) {
      branching ++= binaryFirstFail(flatLocations)
    }

    branching
  }

  onSolution {
    bestObjective =  best(objective, bestObjective)
    bestObjective1 = best(objective1, bestObjective1)
    bestObjective2 = best(objective2, bestObjective2)
    bestObjective3 = best(objective3, bestObjective3)
    currentSolution = new CPIntSol(flatWorkers.map(_.value), bestObjective, 0L)
    emitSolution(createSolution())
  }


  def setInitialSolution(solution: Solution): Unit = {

    val values = Array.tabulate(T, D)((t, d) => {
      val demand = demands(d)

      if (demand.occurs(t)) {
        Array.fill(demand.requiredWorkers)(0)
      }
      else {
        Array[Int]()
      }
    })


    bestObjective = solution.objective
    objective.updateMax(bestObjective)
    for (demandAssignment <- solution.plannings) {
      val d = demandAssignment.demand
      val workerAssignments = demandAssignment.workerAssignments
      for (assignment <- workerAssignments) {
        val t = assignment.timeslot
        val workers = assignment.workers
        for (i <- workers.indices) {
            values(t)(d)(i) = workers(i)
        }
      }
    }

    currentSolution = new CPIntSol(values.flatten.flatten, solution.objective, 0L)
  }

  private def buildToFlat(): Array[Map[Int, Array[Int]]] = {
    val array: Array[Map[Int, Array[Int]]] = Array.fill(T)(null)
    var i = 0
    for (t <- Periods) {
      var map = Map[Int, Array[Int]]()
      for (d <- Demands if demands(d).occurs(t)) {
        val positions = Array.fill(demands(d).requiredWorkers)(0)
        for (p <- demands(d).positions) {
          positions(p) = i
          i += 1
        }

        map = map.updated(d, positions)
      }

      array(t) = map
    }

    array
  }


  override def solve(
     timeLimit: Int = Int.MaxValue,
     solutionLimit: Int = Int.MaxValue,
     silent: Boolean = false,
     options: Option[LNSOptions] = Some(LNSOptions())
   ): SearchResult = {

    solver.silent = silent

    val opt = if (options.isDefined) options.get else LNSOptions()
    val repeat = opt.repeat
    val bestWorkingViolations = opt.bestWorkingViolations

    solver.minimize(objective1, objective2, objective3, objective)

    updateTightenMode(objective1, TightenType.NoTighten)
    updateTightenMode(objective2, TightenType.NoTighten)
    updateTightenMode(objective3, TightenType.NoTighten)
    updateTightenMode(objective, TightenType.StrongTighten)


    val runningTime = time {
      val timeLimitMs: Long = timeLimit.toLong * 1000l
      val limit = opt.limit
      var totalTime = 0L
      var totalSol = 0

      val stat =
        if (currentSolution != null)
          startSubjectTo(nSols = 1, timeLimit = timeLimit) {
            relax(bestWorkingViolations)
          }
        else
          start(nSols = 1, timeLimit = timeLimit)

      totalSol = stat.nSols
      totalTime += stat.time

      var r = 0

      var found = totalSol > 0

      while (bestObjective > objective.min && r < repeat && totalTime < timeLimitMs && totalSol < solutionLimit) {
        val remainingTime = timeLimitMs - totalTime

        val stat = startSubjectTo(nSols = solutionLimit - totalSol, failureLimit = limit, timeLimit = (remainingTime / 1000.0).round.toInt) {
          tightenObjectives(bestWorkingViolations)
          relax(bestWorkingViolations)
        }

        totalSol += stat.nSols
        totalTime += stat.time

        if (timeLimitMs - totalTime < 200) {
          totalTime = timeLimitMs
        }

        found = stat.nSols > 0

        r += 1
      }

    }

    // TODO: does oscar return if it's optimal ?
    SearchResult(lastSolution, runningTime, optimal = false)
  }

  private def updateTightenMode(objective: CPIntVar, tightenMode: TightenType.Value): Unit = {
    solver.obj(objective).tightenMode = tightenMode
  }

  private def tightenObjectives (bestWorkingViolations: Int): Unit = {
    if (bestObjective2 <= 0)
      updateTightenMode(objective2, TightenType.WeakTighten)
    else
      updateTightenMode(objective2, TightenType.StrongTighten)

    if (bestObjective3 > bestWorkingViolations) {
      updateTightenMode(objective3, TightenType.StrongTighten)
      updateTightenMode(objective1, TightenType.NoTighten)
    }
    else {
      updateTightenMode(objective3, TightenType.WeakTighten)
      updateTightenMode(objective1, TightenType.StrongTighten)
    }
  }

  private def relax(bestWorkingViolations: Int): Unit = {
    if (bestObjective3 <= bestWorkingViolations) {
      relaxShifts(currentSolution, 80)
      relaxation.propagationGuidedRelax(solver, flatWorkers, currentSolution, flatWorkers.length / 3)
    }
    else {
      val percentage = 10
      val size = (flatWorkers.length / 100) * percentage
      println(s"Percentage: $percentage, totalSize: ${flatWorkers.length}, size: $size")
      RelaxationFunctions.randomRelax(solver, flatWorkers, currentSolution, size)
    }
    println(flatWorkers.count(_.isBound) + " / " + flatWorkers.length)
  }

  private def relaxShifts(solution: CPIntSol, percentage: Int): Unit = {
    val prob = percentage.toDouble / 100.0
    for (d <- Demands; p <- demands(d).positions) {
      val variables: Set[Int] = demands(d).periods.map(toFlat(_)(d)(p))
      val values: Set[Int] = variables.map(solution.values(_))
      val unique = values.size == 1
      if (unique && Random.nextDouble() < prob) {
        for (v <- variables) {
          add(flatWorkers(v) === values.head)
        }
      }
    }
  }

  private def best(objective: CPIntVar, current: Int) = math.min(objective.value, current)
}






















object MainLNS extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem2.json"
  val generatedFolder = s"$folder/generated/"
  val generatedInstances: Array[String] = Array(
    "t5d5w20-491.json",
    "t10d50w300-638.json"
  )

  val generatedInstancesPath = generatedInstances.map(f => s"$generatedFolder/$f")

  val id = 1
  val path = generatedInstancesPath(id)
  val name = generatedInstances(id)

  val generator = new InstanceGenerator()

  val options = InstanceOptions(
    t = 15,
    clients = 10,
    demands = 50,
    workers = 300,
    skills = 10
  )

  val problem = generator.generate(
    options.copy(probabilities = options.probabilities.updated("assignWorkingRequirements", 0.1))
  )


    //  val problem = JsonParser.parse(path)

    val search = new VillageOneLNS(problem)

//    search.relax {
//      val relaxation = new PropagationGuidedRelaxation()
//      () => {
//        relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 2)
//      }
//    }




    val stats = search.solve(timeLimit = 60)

    val solution = search.lastSolution

    solution match {
      case Some(s) =>
        JsonSerializer.serialize(s)(s"data/results/$name-o=${s.objective}.json")
        println(s.valid)
        println("Partial: " + s.partial)
      case _ => println("No solution found")
    }
}

