package village1.generator


case class InstanceOptions(
  t: Int,
  clients: Int,
  demands: Int,
  workers: Int,
  skills: Int,
  locations: Int = 0,
  machines: Int = 0
)