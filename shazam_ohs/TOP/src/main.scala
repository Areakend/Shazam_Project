import scala.math.{Pi,cos,sin,cosh,sinh,abs,sqrt,log}
object main extends App {
  
  def exp(z:complexes):complexes = {
    val t = (cosh(z.reel) + sinh(z.reel))
    return complexes(cos(z.imaginaire), sin(z.imaginaire)) * t
  }
  
  //Partie FFT Internet

  def fftNET(f: List[complexes]) : List[complexes] = {
    f.size match {
      case 0 => Nil
      case 1 => f
      case n => {
        val c: Double => complexes = phi => complexes(cos(phi), sin(phi))
        val e = fftNET(f.zipWithIndex.filter(_._2%2==0).map(_._1))
        val o  = fftNET(f.zipWithIndex.filter(_._2%2!=0).map(_._1))
        def it(in:List[(Int, complexes)], k:Int = 0) : List[(Int, complexes)] = {
          k < (n / 2) match {
            case true => it( (k+n/2,e(k)-o(k)*c(-2*Pi*k/n)) :: (k,e(k)+o(k)*c(-2*Pi*k/n)) :: in, k + 1)
  	        case false => in
          }
        }
        it(List[(Int, complexes)]()).sortWith((x,y) => x._1 < y._1).map(_._2)
      }
    } 
  
  }
  
  
  // Partie FFT Thibault, à vérifier

  // Fonction séparant les indices pairs et impairs

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
  
  // Fonction qui crée un tableau à partir d'un tableau de tuple
  
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
  
  
  // Partie des tests
  
  var tab:Array[complexes]=Array(complexes(1.0,0.0),complexes(2.0,0.0),complexes(3.0,0.0),complexes(4.0,0.0),complexes(5.0,0.0),complexes(6.0,0.0),complexes(7.0,0.0),complexes(8.0,0.0),complexes(9.0,0.0),complexes(10.0,0.0),complexes(11.0,0.0),complexes(12.0,0.0),complexes(13.0,0.0),complexes(14.0,0.0),complexes(15.0,0.0),complexes(16.0,0.0))
  var list:List[complexes]=List(complexes(1.0,0.0),complexes(2.0,0.0),complexes(3.0,0.0),complexes(4.0,0.0),complexes(5.0,0.0),complexes(6.0,0.0),complexes(7.0,0.0),complexes(8.0,0.0),complexes(9.0,0.0),complexes(10.0,0.0),complexes(11.0,0.0),complexes(12.0,0.0),complexes(13.0,0.0),complexes(14.0,0.0),complexes(15.0,0.0),complexes(16.0,0.0))
  var test=fft(tab)
  var n=tab.length
  
  for (i<-0 to n-1) {println(test(i))} 
  

  

 


 
}