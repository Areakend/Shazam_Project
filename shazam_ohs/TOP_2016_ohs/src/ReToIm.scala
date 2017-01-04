/* La fonction ReToIm prend en entr�e un tableau d'amplitude (r�cup�r� � partir du tableau wav2D d'une musique)
 * et renvoie le tableau de ces m�mes valeurs, mais sous le type complexes.
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