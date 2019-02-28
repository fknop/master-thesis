package village1.benchmark

import org.jfree.chart.axis.{CategoryAxis, CategoryLabelPositions, NumberAxis}
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.renderer.category.StatisticalLineAndShapeRenderer
import org.jfree.chart.{ChartFrame, JFreeChart}
import org.jfree.data.statistics.DefaultStatisticalCategoryDataset
import village1.generator.InstanceGenerator
import village1.modeling.VillageOneModel
import village1.modeling.mip.{SolverResult, VillageOneMIPModel}
import village1.search.cp.VillageOneSearch
import village1.util.Benchmark._



object SolverBenchmark extends App {

  val results = run(repeat = 1, dryRun = 0)

  val dataset = MyChartApp.createDataset(results(0))
  val plot = MyChartApp.createPlot(dataset)
  MyChartApp.show(plot)

  def run (repeat: Int = 10, dryRun: Int = 1, timeLimit: Int = 60): Array[(BenchmarkResult, BenchmarkResult)] = {
    val T = Array(5) //, 10, 15)
    val D = Array(5, 10, 20, 30, 40, 50)
    val W = Array(50, 100, 150, 200, 250, 300)


    val cp = new {
      val measurements = Array.fill(T.length, D.length, W.length)(Array.fill(repeat)(0L))
      val results = Array.fill(T.length)(null)
    }

    val mip = new {
      val measurements = Array.fill(T.length, D.length, W.length)(Array.fill(repeat)(0L))
      val results = Array.fill(T.length)(null)
    }

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


    for (r <- -dryRun until repeat) {
      val measure = r >= 0
      for (i <- T.indices) {
        val t = T(i)
        for (j <- D.indices) {
          val d = D(j)
          for (k <- W.indices) {
            val w = W(k)
            val baseModel = baseModels(i)(j)(k)
            val cpTime = solveCP(baseModel, timeLimit)
            val mipTime = solveMIP(baseModel, timeLimit)

            if (measure) {
              cp.measurements(i)(j)(k)(r) = cpTime
              mip.measurements(i)(j)(k)(r) = mipTime
            }
          }
        }
      }
    }

    val results = Array.fill[(BenchmarkResult, BenchmarkResult)](T.length)(null)
    for (i <- T.indices) {
      val cpValues = Array.fill[(ProblemSize, BenchmarkMeasurement)](D.length * W.length)(null)
      val mipValues = Array.fill[(ProblemSize, BenchmarkMeasurement)](D.length * W.length)(null)
      for (j <- D.indices) {
        for (k <- W.indices) {
          val problemSize = ProblemSize(D(j), W(k))
          val cpMeasurement = getMeasurements(cp.measurements(i)(j)(k))
          val mipMeasurement = getMeasurements(mip.measurements(i)(j)(k))
          cpValues(j) = (problemSize, cpMeasurement)
          mipValues(j) = (problemSize, mipMeasurement)
        }
      }

      results(i) = (BenchmarkResult(T(i), cpValues), BenchmarkResult(T(i), mipValues))
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

  private def solveMIP (base: VillageOneModel, timeLimit: Int): Long = {
    val model = new VillageOneMIPModel(base)
    model.initialize()
    val solver: SolverResult = model.solve(consoleLog = false)
    solver.dispose()

    solver.solveTime
  }

  private def solveCP (base: VillageOneModel, timeLimit: Int): Long = {
    val search = new VillageOneSearch(base)
    val stats = search.solve(1, timeLimit)
    stats.time
  }
}



object MyChartApp extends App {

  def createDataset(results: (BenchmarkResult, BenchmarkResult)): DefaultStatisticalCategoryDataset = {
    val dataset = new DefaultStatisticalCategoryDataset()

    val (cp, mip) = results

    for ((size, stats) <- cp.values) {
      dataset.add(stats.mean, stats.stdev, "CP", size)
    }

    for ((size, stats) <- mip.values) {
      dataset.add(stats.mean, stats.stdev, "MIP", size)
    }

    dataset
  }

  def createPlot(dataset: DefaultStatisticalCategoryDataset): CategoryPlot = {

    val categoryAxis = new CategoryAxis("Instance Size")
    categoryAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_90)
    categoryAxis.setCategoryMargin(2)
    categoryAxis.setMaximumCategoryLabelLines(10)
    categoryAxis.setMaximumCategoryLabelWidthRatio(2.5f)

    val plot = new CategoryPlot(dataset,
      categoryAxis, new NumberAxis("Runtime (ms)"),
      new StatisticalLineAndShapeRenderer())

    plot
  }

  def show(plot: CategoryPlot): Unit = {
    val chart = new JFreeChart(plot)

    val frame = new ChartFrame("Results", chart)
    frame.setSize(1920, 1080)
    frame.setVisible(true)
  }
}