import scala.math.{Pi,cos,sin,cosh,sinh,abs}
object fft {
		
	def exp(z:complexes):complexes = {
				val t = (cosh(z.reel) + sinh(z.reel))
				complexes(cos(z.imaginaire), sin(z.imaginaire)) * t
			}
	  
		  def _fft(cSeq: Seq[complexes], direction: complexes, scalar: Int): Seq[complexes] = {
		    if (cSeq.length == 1) {
		        return cSeq
		    }
		    val n = cSeq.length
		    assume(n % 2 == 0, "The Cooley-Tukey FFT algorithm only works when the length of the input is even.")
	 
	    val evenOddPairs = cSeq.grouped(2).toSeq
	    val evens = _fft(evenOddPairs map (_(0)), direction, scalar)
	    val odds  = _fft(evenOddPairs map (_(1)), direction, scalar)
	 
	    def leftRightPair(k: Int): Pair[complexes, complexes] = {
	        val base = evens(k) / scalar
	        val offset = exp(direction * (Pi * k / n)) * odds(k) / scalar
	        (base + offset, base - offset)
	    }
	 
	    val pairs = (0 until n/2) map leftRightPair
	    val left  = pairs map (_._1)
	    val right = pairs map (_._2)
	    left ++ right
	}
	 
	def  fft(cSeq: Seq[complexes]): Seq[complexes] = _fft(cSeq, complexes(0,  2), 1)
	def rfft(cSeq: Seq[complexes]): Seq[complexes] = _fft(cSeq, complexes(0, -2), 2)
}