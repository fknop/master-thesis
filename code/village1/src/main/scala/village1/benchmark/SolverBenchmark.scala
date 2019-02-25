package village1.benchmark

import java.awt.Font
import java.util.Comparator

import org.jfree.chart.renderer.category.{StackedAreaRenderer, StackedBarRenderer, StatisticalLineAndShapeRenderer}
import village1.generator.InstanceGenerator
import village1.modeling.{Problem, VillageOneModel}
import village1.modeling.mip.{SolverResult, VillageOneMIPModel}
import village1.search.cp.VillageOneSearch
import org.jfree.chart.{ChartFactory, ChartFrame, ChartPanel, JFreeChart}
import org.jfree.chart.axis.{CategoryAxis, CategoryLabelPositions, NumberAxis}
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.ui.RectangleInsets
import org.jfree.data.statistics.DefaultStatisticalCategoryDataset
import village1.util.Benchmark._

case class BenchmarkMeasurement(mean: Double, stdev: Double)

case class ProblemSize(D: Int, W: Int) extends Comparable[ProblemSize] {

  override def compareTo(o: ProblemSize): Int = {
    if (D > o.D) 1
    else if (D < o.D) -1
    else if (W > o.W) 1
    else if (W < o.W) -1
    else 0
  }

  override def toString: String = s"D=$D\nW=$W"
}

case class BenchmarkResult(T: Int, values: Array[(ProblemSize, BenchmarkMeasurement)])


object SolverBenchmark extends App {

  val results = run(repeat = 1, startRun = 0)

  val dataset = MyChartApp.createDataset(results(0))
  val plot = MyChartApp.createPlot(dataset)
  MyChartApp.show(plot)

  def run (repeat: Int = 10, startRun: Int = 1, timeLimit: Int = 60): Array[(BenchmarkResult, BenchmarkResult)] = {
    val T = Array(5) //, 10, 15)
    val D = Array(5, 10, 20, 30, 40, 50)
    val W = Array(50, 100, 150, 200, 250, 300)


    val cp = new {
      val min = Array.fill(T.length, D.length, W.length)(Long.MaxValue)
      val max = Array.fill(T.length, D.length, W.length)(0L)
      val measurements = Array.fill(T.length, D.length, W.length)(Array.fill(repeat)(0L))
      val avg = Array.fill(T.length, D.length, W.length)(0.0)
      val stdev = Array.fill(T.length, D.length, W.length)(0.0)
      val results = Array.fill(T.length)(null)
    }

    val mip = new {
      val min = Array.fill(T.length, D.length, W.length)(Long.MaxValue)
      val max = Array.fill(T.length, D.length, W.length)(0L)
      val measurements = Array.fill(T.length, D.length, W.length)(Array.fill(repeat)(0L))
      val avg = Array.fill(T.length, D.length, W.length)(0.0)
      val stdev = Array.fill(T.length, D.length, W.length)(0.0)
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


    for (r <- -startRun until repeat) {
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

    var results = Array[(BenchmarkResult, BenchmarkResult)]()
    for (i <- T.indices) {
      var cpValues = Array[(ProblemSize, BenchmarkMeasurement)]()
      var mipValues = Array[(ProblemSize, BenchmarkMeasurement)]()
      for (j <- D.indices) {
        for (k <- W.indices) {


          val cpMeasurements = cp.measurements(i)(j)(k)
          val mipMeasurements = mip.measurements(i)(j)(k)

          cp.avg(i)(j)(k) = mean(cpMeasurements)
          cp.min(i)(j)(k) = cpMeasurements.min
          cp.max(i)(j)(k) = cpMeasurements.max
          cp.stdev(i)(j)(k) = stdDev(cpMeasurements)

          mip.avg(i)(j)(k) = mean(mipMeasurements)
          mip.min(i)(j)(k) = mipMeasurements.min
          mip.max(i)(j)(k) = mipMeasurements.max
          mip.stdev(i)(j)(k) = stdDev(mipMeasurements)


          val cpMin = cp.min(i)(j)(k)
          val mipMin = mip.min(i)(j)(k)
          val cpAvg = cp.avg(i)(j)(k)
          val mipAvg = mip.avg(i)(j)(k)
          val cpMax = cp.max(i)(j)(k)
          val mipMax = mip.max(i)(j)(k)

          val problemSize = ProblemSize(D(j), W(k))
          val cpMeasurement = BenchmarkMeasurement(cp.avg(i)(j)(k), cp.stdev(i)(j)(k))
          val mipMeasurement = BenchmarkMeasurement(mip.avg(i)(j)(k), mip.stdev(i)(j)(k))
          cpValues :+= (problemSize, cpMeasurement)
          mipValues :+= (problemSize, mipMeasurement)

          println(s"cp.min: $cpMin, cp.avg: $cpAvg, cp.max: $cpMax")
          println(s"mip.min: $mipMin, mip.avg: $mipAvg, mip.max: $mipMax")
        }
      }

      results :+= (BenchmarkResult(T(i), cpValues), BenchmarkResult(T(i), mipValues))
    }

    results
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