import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt}
object empreinte {
  import com.tncy.top.files.WavWrapper;
  import com.tncy.top.files.Utils;
  var directoryPath : String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Chansons_completes_Mono_11025Hz";
  var files : Array[String] = Utils.listFiles(directoryPath);
  var BDD : Array[Array[Array[Int]]] = Array(Array(Array()))
  for (i<-0 to (files.length -1)) {
    var filePath : String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Chansons_completes_Mono_11025Hz/" + files(i);     
    var wrappedWav : WavWrapper = new WavWrapper(filePath);
    var wav2D : Array[Array[Int]] = wrappedWav.getWav();
    if (i==0) {
      BDD(0) = wav2D
      }
    else {
      BDD = BDD ++ Array(wav2D) 
      }
  }
  
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
 
  def Fempreinte(wav2D : Array[Array[Int]] = Array(Array(),Array())) : Array[Array[Array[Double]]] = {
    
  var indices : Array[Int] = Array();
  //var wav2D : Array[Array[Int]] = Array(Array(10),Array(10));
  var frequence : Array[Double] = fftr(fft(ReToIm(wav2D(1))));
  var amplitude : Array[Int] = wav2D(1);
  var empreinte : Array[Array[Array[Double]]] = Array(Array(Array()));
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
    empreinte = empreinte ++ Array( Array(Array(freq,freq2,(indices(i+1)-indices(i))*dt), Array(indices(i)*dt))) //*dt pour definir le temps et non interval d'indices
  }
  return empreinte
  }
  
  def mememusique(i : Int, E:Array[Array[Array[Double]]]): Int ={
    var triplet : Array[Double] = E(0)(0)
    var M : Array[Array[Array[Double]]] = Fempreinte(BDD(i)) //empreinte de la musique i
    var lm : Int = M.length
    var le = E.length
    var T1 : Array[Double] = Array(-1)
    var o : Int = 0
    var n : Int = 0
    var f : Int = BDD(i)(0)(0) //frequence de la musique
    
    for (j<-0 to (lm -1)) {
      if (triplet == M(j)(0)) {
        T1(0) = 0
        T1 = T1 ++ Array(M(j)(1)(0)) //tableau des occurences de la premiere empreinte
      }
    }
    if (T1(0) == -1 ) {
      n = n + 1
    }
    else {
      o = o + 1
      for (k<-1 to T1.length -1) {
        for (j<- 1 to (le -1)) {      
          var t : Double = E(j)(1)(0) + T1(k)
         // var t2 : Double = t/f  
          for (m<-0 to lm -1) {
            if (t == M(m)(1)) {
              triplet = E(j)(0)
              if (triplet == M(m)(0)) {
                o = o +1
              }
              else {
                n= n +1
              } 
            }
            else {
              n = n+1
            }
          }
          }
        }
    }
    return (o / (o+n) )    
  }
  
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}