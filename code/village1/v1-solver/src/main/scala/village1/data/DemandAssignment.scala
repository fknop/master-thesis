package village1.data

// Assignment for demands
case class DemandAssignment(
  demand: Int,
  workerAssignments: Seq[WorkerAssignment],
  machineAssignments: Option[Array[Int]] = None,
  locationAssignment: Option[Int] = None
)