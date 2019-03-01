package village1.benchmark

case class BenchmarkMeasurement(min: Double, max: Double, mean: Double, stdev: Double)

case class ProblemSize(D: Int, W: Int) extends Comparable[ProblemSize] {

  override def compareTo(o: ProblemSize): Int = {
    if (D > o.D) 1
    else if (D < o.D) -1
    else if (W > o.W) 1
    else if (W < o.W) -1
    else 0
  }

  override def toString: String = s"D=$D\nW=$W"
}

case class BenchmarkResult(size: ProblemSize, time: BenchmarkMeasurement, objective: BenchmarkMeasurement)
case class BenchmarkInstance(T: Int, results: Array[BenchmarkResult])
