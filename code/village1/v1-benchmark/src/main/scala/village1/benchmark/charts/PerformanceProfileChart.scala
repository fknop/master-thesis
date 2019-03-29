package village1.benchmark.charts

import village1.util.FileUtils

object PerformanceProfileChart {

  val ObjectiveTitle = "Objective ratio (lower the better)"
  val TimeTitle = "Time"

  def generate(data: String, xTitle: String = ObjectiveTitle): String => Unit = {
    val path = ChartPaths.PerformanceProfile
    val template = FileUtils.readFile(path)

    val content = ChartUtils.replaceData(template, data)
        .replace("X_AXIS_TITLE", xTitle)

    out: String => {
      FileUtils.writeFile(out, content)
    }
  }
}
