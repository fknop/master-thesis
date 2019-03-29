package village1.benchmark.charts

import village1.util.FileUtils

object ColumnChart {
  def generate(data: String): String => Unit = {
    val path = ChartPaths.Line
    val template = FileUtils.readFile(path)
    val content = ChartUtils.replaceData(template, data)
    out: String => {
      FileUtils.writeFile(out, content)
    }
  }
}
