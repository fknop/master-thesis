package village1

object Util {

  def generatePermutationsOfTwo (size: Int): IndexedSeq[(Int, Int)] = {
    for (i <- 0 until size; j <- i + 1 until size) yield (i, j)
  }


}
