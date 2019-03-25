package village1.search.cp

import oscar.algo.search.Branching
import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.json.JsonSerializer
import village1.modeling.cp.{CPModelOptions, VillageOneCPModel}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.search.{Search, SearchResult}
import village1.util.SysUtils.time

trait Relaxation {
  def apply(): Unit
}

case class LNSOptions(repeat: Int = Int.MaxValue)

class VillageOneLNS(problem: Problem, options: CPModelOptions = CPModelOptions(), base: Option[VillageOneModel] = None)
      extends VillageOneCPModel(problem, options, base)
      with Search[LNSOptions] {

  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))
  def this(base: VillageOneModel, options: CPModelOptions) = this(base.problem, options, Some(base))


  val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
  val flatMachines: Array[CPIntVar] = machineVariables.flatten
  val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)


  solver.addDecisionVariables(flatWorkers)
  solver.addDecisionVariables(flatMachines)
  solver.addDecisionVariables(flatLocations)

  var currentSolution: CPIntSol = _
  var bestObjective: Int = Int.MaxValue

  private var relaxation: Relaxation = () => RelaxationFunctions.randomRelax(solver, flatWorkers, currentSolution, flatWorkers.length / 2)
  private val defaultHeuristic = new MostAvailableHeuristic(this, flatWorkers)
  private var heuristic: Branching = defaultHeuristic.branching

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

  def relax(relaxation: Relaxation): Unit = {
    this.relaxation = relaxation
  }

  def heuristic(branching: Branching): Unit = {
    this.heuristic = branching
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

    minimize(objective)
    search {
      var branching = heuristic

      if (flatMachines.nonEmpty) {
        branching ++= binaryFirstFail(flatMachines)
      }

      if (flatLocations.nonEmpty) {
        branching ++= binaryFirstFail(flatLocations)
      }

      branching
    }

    onSolution {
      currentSolution = new CPIntSol(flatWorkers.map(_.value), objective.value, 0L)
      bestObjective = objective.value
      emitSolution(createSolution())
    }


    val runningTime = time {
      val timeLimitMs = timeLimit * 1000
      var limit = 1000
      var totalTime = 0L
      var totalSol = 0

      val stat =
        if (currentSolution != null)
          startSubjectTo(nSols = 1, timeLimit = timeLimit) {
            relaxation()
          }
        else
          start(nSols = 1, timeLimit = timeLimit)

      totalSol = stat.nSols
      totalTime += stat.time

      var r = 0

      var found = true

      while (bestObjective > objective.min && r < repeat && totalTime < timeLimitMs && totalSol < solutionLimit) {
        val remainingTime = timeLimitMs - totalTime
        val stat = startSubjectTo(nSols = solutionLimit - totalSol, failureLimit = limit, timeLimit = (remainingTime / 1000.0).round.toInt) {
          relaxation()
        }

        totalSol += stat.nSols
        totalTime += stat.time

        if (timeLimitMs - totalTime < 200) {
          totalTime = timeLimitMs
        }

        found = stat.nSols > 0
        limit =
          if (stat.completed || found)
            limit / 2
          else
            if (limit * 2 < 0)
              Int.MaxValue
            else
              limit * 2

        r += 1
      }

    }

    // TODO: does oscar return if it's optimal ?
    SearchResult(lastSolution, runningTime, optimal = false)
  }
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

  val problem = generator.generate(
    InstanceOptions(
      t = 10,
      clients = 10,
      demands = 50,
      workers = 300,
      skills = 10
    )
  )


    //  val problem = JsonParser.parse(path)

    val search = new VillageOneLNS(problem)

//    search.relax {
//      val relaxation = new PropagationGuidedRelaxation()
//      () => relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 3)
//    }

    val stats = search.solve(timeLimit = 3)

    val solution = search.lastSolution

    solution match {
      case Some(s) =>
        JsonSerializer.serialize(s)(s"data/results/$name-o=${s.objective}.json")
        println(s.valid)
        println("Partial: " + s.partial)
      case _ => println("No solution found")
    }
}

