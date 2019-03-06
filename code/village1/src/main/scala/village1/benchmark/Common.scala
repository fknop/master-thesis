package village1.benchmark

case class BenchmarkMeasurement(min: Double, max: Double, mean: Double, stdev: Double)

case class ProblemSize(T: Int, D: Int, W: Int) extends Comparable[ProblemSize] {

  override def compareTo(o: ProblemSize): Int = {
    if (T > o.T) 1
    else if (T < o.T) -1
    else if (D > o.D) 1
    else if (D < o.D) -1
    else if (W > o.W) 1
    else if (W < o.W) -1
    else 0
  }

  override def toString: String = s"T=$T\nD=$D\nW=$W"
}

case class BenchmarkResult(size: ProblemSize, time: BenchmarkMeasurement, objective: BenchmarkMeasurement)
case class BenchmarkSerie(name: String, results: Array[BenchmarkResult])
case class BenchmarkInstance(
    repeat: Int,
    dryRun: Int,
    timeLimit: Int,
    solutionLimit: Int,
    series: Seq[BenchmarkSerie]
)
