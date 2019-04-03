package village1.generator


case class InstanceOptions(
  t: Int,
  clients: Int,
  demands: Int,
  workers: Int,
  skills: Int,
  locations: Int = 0,
  machines: Int = 0,
  probabilities: Map[String, Double] = Map(
    "assignSkill" -> 0.2,
    "assignWorkerSkill" -> 0.2,
    "assignPeriod" -> 0.6,
    "assignLocation" -> 0.5,
    "assignMachines" -> 0.3,
    "takeMachine" -> 0.2,
    "assignWorkingRequirements" -> 0.2
  )
)