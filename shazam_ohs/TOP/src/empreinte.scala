import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt}
object empreinte {
  import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt}
  def Fempreinte(wav2D : Array[Array[Int]] = Array(Array(),Array())) : Array[Array[Double]] = {
    def  fft(t: Array[Int]): Array[complexes] = {
  	   var n : Int = t.length
  	   var l : Array[complexes] = Array()
  	   for (i<-1 to n) {
  	     var y : Int = t(i-1)
  	     l = l ++ Array(complexes(y,0))
  	         }
  	   return(l)
  	 }
    
    def fftr(Tab: Array[complexes]): Array[Double] = {
      var l : Array[Double]= Array()
      var n =Tab.length
      for (y<- 1 to (n)) {
        l = l ++ Array( (Tab(y-1) mod (Tab(y-1))).reel ) 
        }
      return l
  	}
    
    def max(l: Array[Int]) : Int = {
      var m : Int = 0
      for (i<-0 to l.length-1) {
              if (l(i)>m) {m=l(i)}
      }
      return(m);
    }
    
    def module(C: complexes): Double = {
  	  var a = Seq(0)
  	  var b = sqrt(a(0)*(a(0)) + a(1)*(a(1)))
  	  return(b)
  	}
    
  var indices : Array[Int] = Array();
  //var wav2D : Array[Array[Int]] = Array(Array(10),Array(10));
  var frequence : Array[Double] = fftr(fft(wav2D(1)));
  var amplitude : Array[Int] = wav2D(1);
  var empreinte : Array[Array[Double]] = Array(Array());
  var frame = wav2D(0)(0);
  var dt : Double = 1/frame;
  
  //Tableau des indices des grandes amplitudes
  
  for (i<-0 to wav2D(1).length-1) {
    if (wav2D(1)(i) > (7/8)*max(wav2D(1)))
      indices = indices ++ Array(i);
  }
  
  //On récupère les frequences correspondantes
  
  for (i<-0 to (frequence.length/2 -1)) {
    var freq : Double = frequence(2*i);
    var freq2 : Double = frequence(2*i+1);
    empreinte = empreinte ++ Array( Array(freq,freq2,(indices(i+1)-indices(i))*dt, indices(i)*dt)) //*dt pour definir le temps et non interval d'indices
  }
  return empreinte
  }
}