package village1.data

// Planning for one timeslot
case class Planning(
  timeslot: Int,
  demandAssignments: Seq[DemandAssignment]
)