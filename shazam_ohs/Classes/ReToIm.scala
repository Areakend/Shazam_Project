/* La fonction ReToIm prend en entrée un tableau d'amplitude (récupéré à partir du tableau wav2D d'une musique)
 * et renvoie le tableau de ces mêmes valeurs, mais sous le type complexes.
 */

object ReToIm {

  def ReToIm(Tab: Array[Int]): Array[complexes] = {
    var l: Array[complexes] = Array()
    var n = Tab.length
    for (y <- 1 to (n)) {
      l = l ++ Array(complexes(Tab(y - 1), 0))
    }
    return l
  }

}