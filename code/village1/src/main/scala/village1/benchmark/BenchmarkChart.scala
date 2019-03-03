package village1.benchmark

import org.jfree.chart.{ChartFrame, JFreeChart}
import org.jfree.chart.axis.{CategoryAxis, CategoryLabelPositions, NumberAxis}
import org.jfree.chart.plot.{CategoryPlot, CombinedDomainCategoryPlot}
import org.jfree.chart.renderer.category.StatisticalLineAndShapeRenderer
import org.jfree.data.statistics.DefaultStatisticalCategoryDataset


object BenchmarkChart {

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