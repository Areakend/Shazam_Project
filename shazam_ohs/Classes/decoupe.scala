/* La fonction decoupe prend en entrée un tableau wav2D d'une musique et découpe ce dernier en différents
 * tableaux de 1024 valeurs pour optimiser ensuite l'utilisation de l'algorithme FFT
 */

object decoupe {

  def decoupe(tab: Array[Array[Int]]): Array[Array[Int]] = {
    var t: Array[Int] = tab(1)
    var compt: Int = 0
    var n: Int = t.length
    var ni: Int = n / 1024
    var res: Array[Array[Int]] = Array(Array())
    res(0) = tab(0)
    for (i <- 0 to (ni - 1)) {
      var temp: Array[Int] = Array()
      for (k <- 0 to 1024 - 1) {
        temp = temp ++ Array(t(i * 1024 + k))
      }
      res = res ++ Array(temp)
    }
    return res
  }

}