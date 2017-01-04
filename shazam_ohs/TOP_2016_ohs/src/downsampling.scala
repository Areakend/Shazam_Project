/* La fonction downsampling prend en entrée un tableau wav2D : une conversion sera effectuée si la fréquence
 * d'échantillonnage de la musique est 22kHz ou 44kHz, pour passer en 11kHz.
 */

object downsampling {

  def downsampling(tab: Array[Array[Int]]): Array[Array[Int]] = {
    var res: Array[Array[Int]] = Array(Array(), Array())
    res(0) = tab(0)
    if (tab(0)(0) == 11025) {
      return tab
    }
    if (tab(0)(0) == 22050) {
      for (i <- 0 to (tab(1).length / 2) - 1) {
        var mean: Int = 0
        for (j <- 0 to 1) {
          mean = mean + tab(1)(i * 2 + j)
        }
        res(1) = res(1) ++ Array(mean / 2)
      }
      return res
    }
    for (i <- 0 to tab(1).length / 4 - 1) {
      var mean: Int = 0
      for (j <- 0 to 3) {
        mean = mean + tab(1)(i * 4 + j)
      }
      res(1) = res(1) ++ Array(mean / 4)
    }
    return res
  }

}