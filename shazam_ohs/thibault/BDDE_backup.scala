import java.io._
import scala.io.Source

object BDDE_backup {

  def BDDE_backup(BDDE: Array[Array[Array[Array[Double]]]], BDD: Array[Array[Array[Int]]]): Unit = {
    var ch_f = ""
    var Hz_f = ""
    for (i <- 0 to BDDE.length - 1) {
      if (BDD(i)(0)(1) == 1) { ch_f = "Mono" }
      if (BDD(i)(0)(1) == 2) { ch_f = "Stereo" }
      if (BDD(i)(0)(0) == 11025) { Hz_f = "11k" }
      if (BDD(i)(0)(0) == 22050) { Hz_f = "22k" }
      if (BDD(i)(0)(0) == 44100) { Hz_f = "44k" }

      var dirPath = "C:/Users/Thibault/Documents/Ecole/shazam_ohs/Musique/" + ch_f + "_" + Hz_f + "/"
      var fichiers: Array[String] = Utils.listFiles(dirPath)
      val pw = new PrintWriter(new File(dirPath + "BDDE_" + fichiers(i) + ".txt"))
      for (j <- 0 to BDDE(i).length - 1) {
        pw.println(BDDE(i)(j)(0)(0))
        pw.println(BDDE(i)(j)(0)(1))
        pw.println(BDDE(i)(j)(0)(2))
        pw.println(BDDE(i)(j)(1)(0))
      }
      pw.close
    }
  }

}