package village1.data

// Planning for one timeslot
case class Planning(
  workerAssignments: Seq[WorkerAssignment],
  locationAssignments: Seq[LocationAssignment],
  machineAssignments: Seq[MachineAssignment],
  demands: Seq[Demand],
  timeslot: Int
)