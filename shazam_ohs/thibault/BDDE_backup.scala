import java.io._
import scala.io.Source

object BDDE_backup {
  
  def BDDE_backup(BDDE: Array[Array[Array[Array[Double]]]]): Unit = {
    for (i <- 0 to BDDE.length - 1) {
      val pw = new PrintWriter(new File("BDDE_" + files(i) + ".txt"))
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