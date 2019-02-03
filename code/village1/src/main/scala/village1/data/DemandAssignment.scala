package village1.data

// Assignment for demands
case class DemandAssignment(
  timeslot: Int,
  demand: Demand,
  workerAssignments: Seq[WorkerAssignment],
  locationAssignments: Seq[LocationAssignment],
  machineAssignments: Seq[MachineAssignment]
)