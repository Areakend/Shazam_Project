
class complexes{
import scala.math.{Pi,cos,sin,cosh,sinh,abs}
import scala.collection.mutable.ArrayBuffer

case class complexes(reel:Double, imaginaire:Double) {  
  
	def -(a:complexes):complexes = complexes(reel - a.reel,imaginaire - a.imaginaire)
			def +(a:complexes):complexes = complexes(reel + a.reel,imaginaire + a.imaginaire)
			def /(a:Double):complexes = complexes(reel / a,imaginaire / a)
			def *(a:complexes):complexes = complexes(reel * a.reel - imaginaire * a.imaginaire,reel * a.imaginaire + imaginaire * a.reel)
			def *(a:Double):complexes = complexes(reel * a, imaginaire * a)

			override def toString(): String = {
					val x = "%1.5f" format reel
							val y = "%1.5f" format abs(imaginaire)
					(x,y) match {
					case (_, "0.00000") => x
					case ("0.00000", _) => y + "i"
					case (_, _) if imaginaire > 0 => x + " + " + y + "i"
					case (_, _) => x + " - " + y + "i"
					}
			}

}

def exp(z:complexes):complexes = {
		val t = (cosh(z.reel) + sinh(z.reel))
				complexes(cos(z.imaginaire), sin(z.imaginaire)) * t
}
}








