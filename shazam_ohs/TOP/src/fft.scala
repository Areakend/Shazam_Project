import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt}
object fft {
	 def  fft(t: Array[Int]): Array[complexes] = {
	   var n : Int = t.length
	   var l : Array[complexes] = Array()
	   for (i<-1 to n) {
	     var y : Int = t(i-1)
	     l = l ++ Array(complexes(y,0))
	         }
	   return(l)
	 }
	
}