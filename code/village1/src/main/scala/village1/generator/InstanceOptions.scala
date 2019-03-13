package village1.generator


case class InstanceOptions(
  t: Int,
  clients: Int,
  demands: Int,
  workers: Int,
  skills: Int,
  locations: Int = 0,
  machines: Int = 0,
  probabilities: Map[String, Double] = Map("skill" -> 0.2, "period" -> 0.6)
)