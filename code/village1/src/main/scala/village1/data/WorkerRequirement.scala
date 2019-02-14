package village1.data

case class WorkerRequirement(
  skills: Array[Skill] = Array(),
  restrictions: Array[Restriction] = Array()
)