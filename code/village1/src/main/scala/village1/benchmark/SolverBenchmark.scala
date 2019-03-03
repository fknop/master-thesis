package village1.benchmark

import org.jfree.chart.axis.{CategoryAxis, CategoryLabelPositions, NumberAxis}
import org.jfree.chart.plot.{CategoryPlot, CombinedDomainCategoryPlot}
import org.jfree.chart.renderer.category.StatisticalLineAndShapeRenderer
import org.jfree.chart.{ChartFrame, JFreeChart}
import org.jfree.data.statistics.DefaultStatisticalCategoryDataset
import village1.generator.InstanceGenerator
import village1.modeling.VillageOneModel
import village1.modeling.mip.{SolverResult, VillageOneMIPModel}
import village1.search.cp.{SearchHeuristic, VillageOneLNS, VillageOneSearch}
import village1.util.Benchmark._


object SolverBenchmark extends App {
  // Benchmark Parameters
  val T = Array(5) //, 10, 15)
  //  val D = Array(5, 10, 20, 30, 40, 50)
  val D = Array(20, 50)
  val W = Array(100, 200, 300)

  val SolutionLimit: Int = Int.MaxValue
  val TimeLimit = 20 // seconds
  val Repeat = 2
  val DryRun = 1

  val baseModels = Array.tabulate(T.length, D.length, W.length) { (t, d, w) =>
    val problem = InstanceGenerator.generate(
      t = T(t),
      c = D(d), // This parameter doesn't really matter
      d = D(d),
      w = W(w),
      s = 10,
      prob = Map("skill" -> 0.2, "period" -> 0.6)
    )

    new VillageOneModel(problem)
  }


  val results = run(repeat = Repeat, dryRun = DryRun, solve = solveCP)
  //  val results2 = run(repeat = 2, dryRun = 1, solve = solveCPDefaultHeuristic)
  val results2 = run(repeat = Repeat, dryRun = DryRun, solve = solveMIP)
  val results3 = run(repeat = Repeat, dryRun = DryRun, solve = solveCPThenMIP)


  val tdataset = BenchmarkChart.createRuntimeDataset(("CP", results(0)), ("MIP", results2(0)), ("CP+MIP", results3(0)))
  val tplot = BenchmarkChart.createRuntimePlot(tdataset)

  val odataset = BenchmarkChart.createObjectiveDataset(("CP", results(0)), ("MIP", results2(0)), ("CP+MIP", results3(0)))
  val oplot = BenchmarkChart.createObjectivePlot(odataset)

  val plot = BenchmarkChart.combinePlot(tplot, oplot)

  BenchmarkChart.show(plot, title = s"Benchmark: T=${results(0).T}")

  def run (repeat: Int = 10, dryRun: Int = 1, solve: (VillageOneModel) => (Long, Int)): Array[BenchmarkInstance] = {

    val timeMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(repeat)(0L))
    val objectiveMeasurements = Array.fill(T.length, D.length, W.length)(Array.fill(repeat)(0L))

    for (r <- -dryRun until repeat) {
      val measure = r >= 0
      for (i <- T.indices) {
        for (j <- D.indices) {
          for (k <- W.indices) {
            val baseModel = baseModels(i)(j)(k)
            val (time, objective) = solve(baseModel)
            if (measure) {
              timeMeasurements(i)(j)(k)(r) = time
              objectiveMeasurements(i)(j)(k)(r) = objective
            }
          }
        }
      }
    }

    val results = Array.fill[BenchmarkInstance](T.length)(null)
    for (i <- T.indices) {
      val values = Array.fill[BenchmarkResult](D.length * W.length)(null)
      var v = 0
      for (j <- D.indices) {
        for (k <- W.indices) {
          val problemSize = ProblemSize(D(j), W(k))
          val timeMeasurement = getMeasurements(timeMeasurements(i)(j)(k))
          val objectiveMeasurement = getMeasurements(objectiveMeasurements(i)(j)(k))
          values(v) = BenchmarkResult(problemSize, timeMeasurement, objectiveMeasurement)
          v += 1
        }
      }

      results(i) = BenchmarkInstance(T(i), values)
    }

    results
  }

  private def getMeasurements(measurements: Array[Long]): BenchmarkMeasurement = {
    BenchmarkMeasurement(
      measurements.min,
      measurements.max,
      mean(measurements),
      stdDev(measurements)
    )
  }

  private def solveCPThenMIP (base: VillageOneModel): (Long, Int) = {
    val cp = new VillageOneSearch(base)
    val stat = cp.solve(nSols = math.max(SolutionLimit / 2, 1), timeLimit = (TimeLimit * 1000) / 2, silent = true)

    val remaining = TimeLimit - (stat.time / 1000).toInt


    val model = new VillageOneMIPModel(base)
    model.initialize(withObjective = true)

    if (cp.lastSolution != null) {
      model.setInitialSolution(cp.lastSolution)
    }

    val solver: SolverResult = model.solve(silent = true, timeLimit = remaining / 2, nSols = math.max(SolutionLimit / 2, 1))
    solver.dispose()
    (solver.solveTime + stat.time, solver.solution.objective)
  }

  private def solveMIP (base: VillageOneModel): (Long, Int) = {
    val model = new VillageOneMIPModel(base)
    model.initialize(withObjective = true)
    val solver: SolverResult = model.solve(silent = true, timeLimit = TimeLimit, nSols = SolutionLimit)
    solver.dispose()

    (solver.solveTime, solver.solution.objective)
  }

  private def solveCP (base: VillageOneModel): (Long, Int) = {
    val search = new VillageOneLNS(base)
    val stats = search.solve(nSols = SolutionLimit, timeLimit = TimeLimit * 1000, silent = true)
    assert(search.lastSolution != null)
    (stats, search.lastSolution.objective)
  }

  private def solveCPDefaultHeuristic (base: VillageOneModel): (Long, Int) = {
    val search = new VillageOneLNS(base)
    val stats = search.solve(nSols = SolutionLimit, timeLimit = TimeLimit * 1000, silent = true, heuristic = SearchHeuristic.Default)
    assert(search.lastSolution != null)
    (stats, search.lastSolution.objective)
  }
}



object BenchmarkChart extends App {

  def createRuntimeDataset(instances: (String, BenchmarkInstance)*): DefaultStatisticalCategoryDataset = {
    val dataset = new DefaultStatisticalCategoryDataset()

    for ((name, instance) <- instances) {
      val results = instance.results
      for (result <- results) {
        val time = result.time
        dataset.add(time.mean, time.stdev, name, result.size)
      }
    }

    dataset
  }

  def createObjectiveDataset(instances: (String, BenchmarkInstance)*): DefaultStatisticalCategoryDataset = {
    val dataset = new DefaultStatisticalCategoryDataset()

    for ((name, instance) <- instances) {
      val results = instance.results
      for (result <- results) {
        val objective = result.objective
        dataset.add(objective.mean, objective.stdev, name, result.size)
      }
    }

    dataset
  }

  def createRuntimePlot(dataset: DefaultStatisticalCategoryDataset): CategoryPlot = {

    val categoryAxis = new CategoryAxis("Instance Size")
    categoryAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_90)
//    categoryAxis.setCategoryMargin(2)
    categoryAxis.setMaximumCategoryLabelLines(10)
//    categoryAxis.setMaximumCategoryLabelWidthRatio(2.5f)

    val plot = new CategoryPlot(dataset,
      categoryAxis, new NumberAxis("Runtime (ms)"),
      new StatisticalLineAndShapeRenderer())

    plot
  }

  def createObjectivePlot(dataset: DefaultStatisticalCategoryDataset): CategoryPlot = {

    val categoryAxis = new CategoryAxis("Instance Size")
    categoryAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_90)
    //    categoryAxis.setCategoryMargin(2)
    categoryAxis.setMaximumCategoryLabelLines(10)
    //    categoryAxis.setMaximumCategoryLabelWidthRatio(2.5f)

    val plot = new CategoryPlot(dataset,
      categoryAxis, new NumberAxis("Objective (minimization)"),
      new StatisticalLineAndShapeRenderer())

    plot
  }

  def combinePlot(plots: CategoryPlot*): CombinedDomainCategoryPlot = {
    val categoryAxis = new CategoryAxis("Instance Size")
    categoryAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_90)
    //    categoryAxis.setCategoryMargin(2)
    categoryAxis.setMaximumCategoryLabelLines(10)
    val plot = new CombinedDomainCategoryPlot(categoryAxis)
    plots.foreach(plot.add)
    plot
  }

  def show(plot: CategoryPlot, title: String = "Benchmark Results"): Unit = {
    val chart = new JFreeChart(plot)

    val frame = new ChartFrame(title, chart)
    frame.setSize(1920, 1080)
    frame.setVisible(true)
  }
}