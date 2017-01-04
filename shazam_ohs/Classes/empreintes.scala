/* Les 2 fonctions suivantes permettent de créer l'empreinte d'une musique donnée
 * 
 */

object empreintes {

  // Certaines fonctions auxiliaires n'étant pas présentes, la fonction est mise en commentaire pour éviter
  // les erreurs.
  
/*  

  def FofInterest(tab: Array[Double]): Array[Double] = {
    var tmax: Array[Double] = Array()
    var max: Double = 0
    max = tab.slice(0, 9).reduceLeft(_ max _)
    tmax = tmax ++ Array(max)
    max = (tab.slice(10, 19).reduceLeft(_ max _))
    tmax = tmax ++ Array(max)
    max = (tab.slice(20, 39).reduceLeft(_ max _))
    tmax = tmax ++ Array(max)
    max = (tab.slice(40, 79).reduceLeft(_ max _))
    tmax = tmax ++ Array(max)
    max = (tab.slice(80, 159).reduceLeft(_ max _))
    tmax = tmax ++ Array(max)
    max = (tab.slice(160, 511).reduceLeft(_ max _))
    tmax = tmax ++ Array(max)
    var indices: Array[Int] = Array()
    indices = indices ++ Array(indice_max(tab.slice(0, 9)).toInt)
    indices = indices ++ Array(indice_max(tab.slice(10, 19)).toInt)
    indices = indices ++ Array(indice_max(tab.slice(20, 39)).toInt)
    indices = indices ++ Array(indice_max(tab.slice(40, 79)).toInt)
    indices = indices ++ Array(indice_max(tab.slice(80, 159)).toInt)
    indices = indices ++ Array(indice_max(tab.slice(160, 511)).toInt)
    var mean: Double = 0.0
    for (j <- 0 to 5) {
      mean = mean + tmax(j)
    }
    mean = mean / 6
    var tres: Array[Double] = Array()
    for (i <- 0 to tmax.length - 1) {
      if (tmax(i) > mean) {
        tres = tres ++ Array(indices(i).toDouble)
      }
    }
    return tres
  }

  def empreinte(tab: Array[Array[Int]]): Array[Array[Array[Double]]] = {
    var test0: Array[Array[Int]] = moyenneStereo(tab)
    var test1: Array[Array[Int]] = downsampling(test0)
    var test2: Array[Array[Int]] = decoupe(test1)
    var freqM: Array[Array[Double]] = fftm(test2)
    var freq: Array[Array[Double]] = MtoC(freqM)
    var fe: Double = tab(0)(0)
    var tmax: Array[Double] = Array()
    var tres: Array[Array[Double]] = Array()
    var emp: Array[Array[Array[Double]]] = Array()
    for (i <- 1 to freq.length - 1) {
      tres = tres ++ Array(FofInterest(freq(i)))
    }
    for (j <- 0 to tres.length - 8) {
      if (tres(j).length != 0) {
        for (k <- 3 to 7) {
          for (m <- 0 to tres(j + k).length - 1) {
            emp = emp ++ Array(Array(Array(tres(j)(0), tres(j + k)(m), k.toDouble), Array(j.toDouble)))
          }
        }
      }
    }
    return emp
  }
   
*/

}