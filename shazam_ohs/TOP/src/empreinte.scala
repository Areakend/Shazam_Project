import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt}
object empreinte {
  import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt}
  def Fempreinte(wav2D : Array[Array[Int]] = Array(Array(),Array())) : Array[Array[Double]] = {

      def ReToIm(Tab: Array[Int]): Array[complexes] = {
    var l : Array[complexes]= Array()
    var n =Tab.length
    for (y<- 1 to (n)) {
      l = l ++ Array( complexes( Tab(y-1),0 ) )
    }
    return l
	}

      def fftr(Tab: Array[complexes]): Array[Double] = {
    var l : Array[Double]= Array()
    var n =Tab.length
    for (y<- 1 to (n)) {
      l = l ++ Array( (Tab(y-1) mod (Tab(y-1))).reel ) 
      }
    return l
	}
      
      def exp(z:complexes):complexes = {
    val t = (cosh(z.reel) + sinh(z.reel))
    return complexes(cos(z.imaginaire), sin(z.imaginaire)) * t
  }
  
    def ImpairPair(tab:Array[complexes]):Array[Array[complexes]] = {
      var tab1:Array[complexes]=Array()
      var tab2:Array[complexes]=Array()
      for (i <- 0 to tab.length-1) {
        if (i%2==0) {
          tab1 = tab1 ++ Array(tab(i))
        } else {
          tab2 = tab2 ++ Array(tab(i))
        }
      }
      return Array(tab1,tab2)
    }
    
    // Fonction qui cr�e un tableau � partir d'un tableau de tuple
    
    def tri(tab:Array[(Int,complexes)]):Array[complexes] = {
      var res:Array[complexes] = Array()
      var n:Int=tab.length
      var el:Int = 0
      var j:Int=0
      var temp:Int=0
      var tuple = tab(0)
      for (i <- 1 to n-1) {
        el = tab(i)._1
        tuple = tab(i)
        j = i
        while (j > 0 && el < tab(j-1)._1) {
          tab(j) = tab(j-1)
          j-=1
        }
        tab(j) = tuple
      }
      for (i <- 0 to tab.length-1) {
        res = res ++ Array( tab(i)._2 )
      }
      res
    }
  
    // FFT 
    
    def fft(tab:Array[complexes]):Array[complexes] = {
        if (tab.length == 0) {Array()}
        if (tab.length == 1) {tab}
        else {
        var n=tab.length
        var tabP = fft( ImpairPair(tab)(0) )  
        var tabIP = fft( ImpairPair(tab)(1) )  
        def Xk(table:Array[(Int, complexes)], k:Int = 0):Array[(Int,complexes)]= {
          var w = exp(complexes(0,-2*Pi*k/n))
          if (k<(n/2)) {
            Xk(  Array( (k+n/2,tabP(k)-tabIP(k)*(w)) ) ++ Array( (k,tabP(k)+tabIP(k)*(w)) ) ++ table , k + 1 )
          } 
          else {table}
        }
          tri( Xk(Array[(Int,complexes)]()) )
        }
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
  var frequence : Array[Double] = fftr(fft(ReToIm(wav2D(1))));
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
  def mememusique(L1:Array[Int], L2:Array[Int]): Boolean ={
   def remove(L:Array[Int],q:Int):Array[Int] ={
     var T:Array[Int]=Array()
     for (i<- 0 to q-1) {
       var temp2 : Array[Int] = Array( L(0) )
       temp2 = temp2 ++ T
       var T = temp2
       var temp : Array[Int] = Array()
       for (i<-1 to L.length -1) {
         temp = temp ++ Array(L(i))
       }
      var L = temp
     }
     T=T ++ L
     return T
   }
     var n=0   // pour arrêter la boucle quand une valeur similaire est trouvée
      var m=0   //nombre de similitudes
      var p=0 // indice de la valeur de L2
    for (k<- 0 to L1.size -1) {
     
      while (p!= L2.size -1 && n==0) {
        if (L1(k)==L2(p)) {
          m+=1
          n=1
          remove(L2,p)
        }
        p+=1
        
      }
    }
    if (m>(90/100)*L2.size) {
      return true
    }
    else {
      return false
    }
  }
  
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}