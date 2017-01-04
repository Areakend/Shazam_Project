/* Les 3 fonctions suivantes permettent d'effectuer la reconnaissance d'une musique à partir de son échantillon.
 * Nous gérons une base de données empreinte sous la forme de fichiers textes.
 */

object reconnaissance {

  // Certaines fonctions auxiliaires n'étant pas présentes, la fonction est mise en commentaire pour éviter
  // les erreurs.
  
/*

  def backuped_reconnaissance_m(wav2D: Array[Array[Int]]): String = {
    var ch_f = ""
    var Hz_f = ""
    if (wav2D(0)(1) == 1) { ch_f = "Mono" }
    if (wav2D(0)(1) == 2) { ch_f = "Stereo" }
    if (wav2D(0)(0) == 11025) { Hz_f = "11k" }
    if (wav2D(0)(0) == 22050) { Hz_f = "22k" }
    if (wav2D(0)(0) == 44100) { Hz_f = "44k" }
    var dirPath = "C:/Users/Thibault/Documents/Ecole/shazam_ohs/Musique/" + ch_f + "_" + Hz_f + "/"
    var dirPathBDDE = "C:/Users/Thibault/Documents/Ecole/shazam_ohs/Musique/BDDE/" + ch_f + "_" + Hz_f + "/"
    var fichiers: Array[String] = Utils.listFiles(dirPath)
    var fichiers_bdde: Array[String] = Utils.listFiles(dirPathBDDE)
    var tab_same: Array[Double] = Array()
    var E: Array[Array[Array[Double]]] = empreinte(wav2D)
    var M: Array[Array[Array[Double]]] = Array()
    for (i <- 0 to fichiers.length - 1) {
      M = FiletoTab(i, fichiers_bdde, dirPathBDDE)
      println("Recherche des occurences des marqueurs la musique " + i + " (il y a " + fichiers.length + " musiques)")
      var temp = E_occur(E, M)
      println("Calcul des similitudes de la musique " + i + " (il y a " + fichiers.length + " musiques)")
      tab_same = tab_same ++ Array(memediff(temp))
    }
    println(tab_same.deep)
    var indice_musique = indice_max(tab_same)
    if (tab_same(indice_musique.toInt) < 250) {
      return ("Musique inconnue")
    }
    return ("La musique est " + fichiers(indice_musique.toInt) + " !")

  }

  def BDDE_backup(BDDE: Array[Array[Array[Array[Double]]]], BDD: Array[Array[Array[Int]]]): Unit = {
    var ch_f = ""
    var Hz_f = ""
    if (BDD(0)(0)(1) == 1) { ch_f = "Mono" }
    if (BDD(0)(0)(1) == 2) { ch_f = "Stereo" }
    if (BDD(0)(0)(0) == 11025) { Hz_f = "11k" }
    if (BDD(0)(0)(0) == 22050) { Hz_f = "22k" }
    if (BDD(0)(0)(0) == 44100) { Hz_f = "44k" }
    var dirPath = "C:/Users/Thibault/Documents/Ecole/shazam_ohs/Musique/" + ch_f + "_" + Hz_f + "/"
    var dirPathBDDE = "C:/Users/Thibault/Documents/Ecole/shazam_ohs/Musique/BDDE/" + ch_f + "_" + Hz_f + "/"
    var fichiers: Array[String] = Utils.listFiles(dirPath)
    for (i <- 0 to fichiers.length - 1) {
      println("Création du fichier empreinte n°" + i.toString)
      val pw = new PrintWriter(new File(dirPathBDDE + "BDDE_" + fichiers(i) + ".txt"))
      for (j <- 0 to BDDE(i).length - 1) {
        pw.println(BDDE(i)(j)(0)(0))
        pw.println(BDDE(i)(j)(0)(1))
        pw.println(BDDE(i)(j)(0)(2))
        pw.println(BDDE(i)(j)(1)(0))
      }
      pw.close
    }
  }

  def FiletoTab(i: Int, fichiers: Array[String], dirPath: String): Array[Array[Array[Double]]] = {
    var res: Array[Array[Array[Double]]] = Array()
    var marq: Array[Array[Double]] = Array()
    var f1 = 0.0
    var f2 = 0.0
    var dt = 0.0
    var t = 0.0
    var cmp = 0
    for (line <- Source.fromFile(dirPath + fichiers(i)).getLines()) {
      if (cmp == 4) {
        cmp = 0
        marq = Array(Array(0.0, 0.0, 0.0), Array(0.0))
      }
      if (cmp == 0) {
        f1 = line.toDouble
        marq = Array(Array(f1, 0.0, 0.0), Array(0.0))
      }
      if (cmp == 1) {
        f2 = line.toDouble
        marq = Array(Array(f1, f2, 0.0), Array(0.0))
      }
      if (cmp == 2) {
        dt = line.toDouble
        marq = Array(Array(f1, f2, dt), Array(0.0))
      }
      if (cmp == 3) {
        t = line.toDouble
        marq = Array(Array(f1, f2, dt), Array(t))
        res = res ++ Array(marq)
      }
      cmp += 1
    }
    res
  }

*/

}