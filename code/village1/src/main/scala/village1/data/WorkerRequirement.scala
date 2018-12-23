package village1.data

case class WorkerRequirement(
  skills: IndexedSeq[Skill] = IndexedSeq(),
  restrictions: IndexedSeq[Restriction] = IndexedSeq()
)