/* Les 2 fonctions suivantes permettent de d�terminer quelle musique correspond � l'�chantillon analys� 
 * parmi une certaine base de donn�es d'empreintes de musiques.
 */

object Match {

  // Certaines fonctions auxiliaires n'�tant pas pr�sentes, la fonction est mise en commentaire pour �viter
  // les erreurs.
  
/*

  def E_occur(E: Array[Array[Array[Double]]], M: Array[Array[Array[Double]]]): Array[Double] = {
    var T = new Array[Double](0)
    var n: Int = E.length - 5
    for (k <- 0 to n) {
      var test: Int = 0
      for (c <- 0 to M.length - 5) {
        if ((E(k)(0).deep == M(c)(0).deep)) {
          T = T ++ Array(M(c)(1)(0) - E(k)(1)(0))
        }
      }
    }
    return T
  }

  def memediff(T: Array[Double]): Double = {
    var res: Array[Int] = Array.fill(T.length)(0)
    for (i <- 0 to T.length - 1) {
      for (j <- 0 to T.length - 1) {
        if (T(i) == T(j)) { res(i) += 1 }
      }
    }
    return max(res)
  }

*/

}