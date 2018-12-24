package village1.data

// Planning for one timeslot
case class Planning(
  workerAssignments: IndexedSeq[WorkerAssignment],
  locationAssignments: IndexedSeq[LocationAssignment],
  machineAssignments: IndexedSeq[MachineAssignment],
  demands: IndexedSeq[Demand],
  timeslot: Int
)