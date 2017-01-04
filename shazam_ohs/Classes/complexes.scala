import scala.math.{ Pi, cos, sin, cosh, sinh, abs, sqrt }

/* D�finition de la classe complexe, n�cessaire pour utiliser l'algorithme FFT (Fast Fourier Transform)
 * Sont ainsi d�finies les op�rations de base, addition, soustraction, etc.. �galement l'exponentielle complexe
*/

case class complexes(reel: Double, imaginaire: Double) {

  def -(a: complexes): complexes = complexes(reel - a.reel, imaginaire - a.imaginaire)
  def +(a: complexes): complexes = complexes(reel + a.reel, imaginaire + a.imaginaire)
  def /(a: Double): complexes = complexes(reel / a, imaginaire / a)
  def *(a: complexes): complexes = complexes(reel * a.reel - imaginaire * a.imaginaire, reel * a.imaginaire + imaginaire * a.reel)
  def *(a: Double): complexes = complexes(reel * a, imaginaire * a)
  def mod(a: complexes): complexes = complexes(sqrt(reel * a.reel + imaginaire * a.imaginaire), 0.00000)

  override def toString(): String = {
    val x = "%1.5f" format reel
    val y = "%1.5f" format abs(imaginaire)
    (x, y) match {
      case (_, "0.00000") => x
      case ("0.00000", _) => y + "i"
      case (_, _) if imaginaire > 0 => x + " + " + y + "i"
      case (_, _) => x + " - " + y + "i"
    }
  }

  def exp(z: complexes): complexes = {
    val t = (cosh(z.reel) + sinh(z.reel))
    return complexes(cos(z.imaginaire), sin(z.imaginaire)) * t
  }

}

