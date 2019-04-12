package village1.benchmark.charts

object ChartUtils {
  def replaceData(template: String, data: String): String = {
    template.replace("{ /** DATA **/ }", data)
  }
}
