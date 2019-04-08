package village1.search.mip

import gurobi.{GRB, GRBException}
import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.json.JsonSerializer
import village1.modeling.mip.{MipModelOptions, VillageOneMIPModel}
import village1.modeling.{Problem, UnsolvableException, VillageOneModel}
import village1.search.cp.VillageOneLNS
import village1.search.{Search, SearchOptions, SearchResult}
import village1.util.SysUtils.time


case class MipSearchOptions(MIPFocus: Int = 1) extends SearchOptions

class MIPSearch(problem: Problem, options: MipModelOptions = MipModelOptions(), base: Option[VillageOneModel] = None)
  extends VillageOneMIPModel(problem, options, base)
  with Search[MipSearchOptions] {

  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))
  def this(base: VillageOneModel, options: MipModelOptions) = this(base.problem, options, Some(base))

  def solve(timeLimit: Int = -1, solutionLimit: Int = Int.MaxValue, silent: Boolean = false, options: Option[MipSearchOptions] = None): SearchResult = {

    if (timeLimit > 0) {
      model.set(GRB.DoubleParam.TimeLimit, timeLimit)
    }

    if (silent) {
      model.set(GRB.IntParam.LogToConsole, 0)
    }

    val opt = if (options.isDefined) options.get else MipSearchOptions()


    model.set(GRB.IntParam.MIPFocus, opt.MIPFocus)
    model.set(GRB.IntParam.SolutionLimit, solutionLimit)


    val solutionListener = new SolutionListener(this)
    solutionListener.onSolutionFound(emitSolution)
    model.setCallback(solutionListener)

    val t = time {
      model.optimize()
    }

    val status = model.get(GRB.IntAttr.Status)

    if (status == GRB.INFEASIBLE) {
      throw new UnsolvableException("Model is infeasible")
    }

    model.dispose()

    SearchResult(lastSolution, t)
  }
}


object MipMain extends App {

  val name = "t10d50w300-638"
  val path = s"data/instances/generated/${name}.json"
//  val problem = JsonParser.parse(path)
  val generator = new InstanceGenerator()

  val problem = generator.generate(
    InstanceOptions(
      t = 15,
      clients = 10,
      demands = 50,
      workers = 300,
      skills = 10
    )
  )

  JsonSerializer.serialize(problem)("./test.json")

  val cpSearch = new VillageOneLNS(problem)

//  val cp = cpSearch.solve(timeLimit = 10)

  val model = new MIPSearch(problem)
  model.model.write("model.mps")

//  model.presolve()


//  if (cp.solution.isDefined) {
//    println(cp.solution.get.valid)
//    model.setInitialSolution(cp.solution.get, 0.5)
//  }


  try {
    val results = model.solve(timeLimit = 60)
    val solution = results.solution.get
    println(solution.valid)
    println(results.time)
    JsonSerializer.serialize(solution)(s"data/results/mip-${name}-o=${solution.objective}.json")
  }
  catch {
    case exception: GRBException => exception.printStackTrace()
  }
}
