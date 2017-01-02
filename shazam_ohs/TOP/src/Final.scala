

object Final {
  import scala.math.{ Pi, cos, sin, cosh, sinh, abs, sqrt }
  import com.tncy.top.files.WavWrapper;
  import com.tncy.top.files.Utils;
  var directoryPath: String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Chansons_completes_Mono_11025Hz";
  var files: Array[String] = Utils.listFiles(directoryPath);
  var BDD: Array[Array[Array[Int]]] = Array(Array(Array()))
  for (i <- 0 to (files.length - 1)) {
    var filePath: String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Chansons_completes_Mono_11025Hz/" + files(i);
    var wrappedWav: WavWrapper = new WavWrapper(filePath);
    var wav2D: Array[Array[Int]] = wrappedWav.getWav();
    if (i == 0) {
      BDD(0) = wav2D
    } else {
      BDD = BDD ++ Array(wav2D)
    }
  }

  def moyenneStereo(wav2D: Array[Array[Int]]): Array[Int] = {
    var mono: Array[Int] = Array()
    if ((wav2D(0)(1) == 2)) {
      mono = wav2D(1) ++ wav2D(2)
      return mono
    } else {
      return wav2D(1)
    }
  }
  
  def decoupe(tab : Array[Array[Int]]) : Array[Array[Int]] = {
    var t : Array[Int] = tab(1)
    var compt : Int = 0
    var n : Int = t.length
    var ni : Int = n/1024
    var res : Array[Array[Int]] = Array(Array())
    for (i<-0 to (ni-1)) {
      var temp : Array[Int] = Array()
      for (k<-0 to 1024-1) {
        temp = temp ++ Array(tab(1)(i+k))
      }
      res = res ++ Array(temp)
    }
    return res
  }

  def wav(musique: String): Array[Array[Int]] = {
    var filePath: String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Echantillons_connus_Mono_11025Hz/" + musique;
    var wrappedWav: WavWrapper = new WavWrapper(filePath)
    var wav2D: Array[Array[Int]] = wrappedWav.getWav()
    return wav2D
  }
  //Test
  //var BDD : Array[Array[Array[Array[Double]]]] =  Array(Array(Array(Array())))
  //BDD = Array(Array( Array( Array( 1250, 3500, 3), Array(0) ), Array( Array(4500, 10580, 4), Array(5)),
  //  Array( Array( 12500, 35000, 4), Array(0) ), Array( Array(45000, 100580, 6), Array(6)) ))
  def ReToIm(Tab: Array[Int]): Array[complexes] = {
    var l: Array[complexes] = Array()
    var n = Tab.length
    for (y <- 1 to (n)) {
      l = l ++ Array(complexes(Tab(y - 1), 0))
    }
    return l
  }

  def fftr(Tab: Array[complexes]): Array[Double] = {
    var l: Array[Double] = Array()
    var n = Tab.length
    for (y <- 1 to (n)) {
      l = l ++ Array((Tab(y - 1) mod (Tab(y - 1))).reel)
    }
    return l
  }

  def exp(z: complexes): complexes = {
    val t = (cosh(z.reel) + sinh(z.reel))
    return complexes(cos(z.imaginaire), sin(z.imaginaire)) * t
  }

  def ImpairPair(tab: Array[complexes]): Array[Array[complexes]] = {
    var tab1: Array[complexes] = Array()
    var tab2: Array[complexes] = Array()
    for (i <- 0 to tab.length - 1) {
      if (i % 2 == 0) {
        tab1 = tab1 ++ Array(tab(i))
      } else {
        tab2 = tab2 ++ Array(tab(i))
      }
    }
    return Array(tab1, tab2)
  }
  // Fonction qui crée un tableau à partir d'un tableau de tuple

  def tri(tab: Array[(Int, complexes)]): Array[complexes] = {
    var res: Array[complexes] = Array()
    var n: Int = tab.length
    var el: Int = 0
    var j: Int = 0
    var temp: Int = 0
    var tuple = tab(0)
    for (i <- 1 to n - 1) {
      el = tab(i)._1
      tuple = tab(i)
      j = i
      while (j > 0 && el < tab(j - 1)._1) {
        tab(j) = tab(j - 1)
        j -= 1
      }
      tab(j) = tuple
    }
    for (i <- 0 to tab.length - 1) {
      res = res ++ Array(tab(i)._2)
    }
    res
  }

  // FFT 

  def fft(tab: Array[complexes]): Array[complexes] = {
    if (tab.length == 0) { Array() }
    if (tab.length == 1) { tab }
    else {
      var n = tab.length
      var tabP = fft(ImpairPair(tab)(0))
      var tabIP = fft(ImpairPair(tab)(1))
      def Xk(table: Array[(Int, complexes)], k: Int = 0): Array[(Int, complexes)] = {
        var w = exp(complexes(0, -2 * Pi * k / n))
        if (k < (n / 2)) {
          Xk(Array((k + n / 2, tabP(k) - tabIP(k) * (w))) ++ Array((k, tabP(k) + tabIP(k) * (w))) ++ table, k + 1)
        } else { table }
      }
      tri(Xk(Array[(Int, complexes)]()))
    }
  }
  
  def fftm(tab : Array[Array[Int]]) : Array[Array[Double]] = {
    var n : Int = tab.length -1
    var res : Array[Array[Double]] = Array(Array())
    for (i<-0 to n) {
      res = res ++ Array(fftr(ReToIm(tab(i))))
    }
    return res
  }
  
  def BDDE(tab : Array[Array[Array[Int]]]) : Array[Array[Array[Array[Double]]]] = {
    
    var bdde : Array[Array[Array[Array[Double]]]] = Array(Array(Array(Array())))
    for (i<-0 to tab.length -1) {
      bdde = bdde ++ Array(empreinte(tab(i)))
    }
    return bdde  
  }
  
  def empreinte(tab : Array[Array[Int]]) : Array[Array[Array[Double]]] = {
    var t1 : Array[Double] = Array()
      var t2 : Array[Double] = Array()
      var t3 : Array[Double] = Array()
      var t4 : Array[Double] = Array()
      var t5 : Array[Double] = Array()
      var t6 : Array[Double] = Array()
      var freq : Array[Array[Double]] = fftm(decoupe(tab))
      var fe : Double = tab(0)(0)
      var tmax :  Array[Double] = Array()
      for (k<-0 to freq.length -1) {
        var max : Double = 0
        for (j<-0 to 9) {
          t1 = t1 ++ Array(freq(k)(j))
          if (freq(k)(j) > max) {
            max = freq(k)(j)
            }
          }
        tmax = tmax ++ Array(max)
        max = 0.0
        for (j<-10 to 19) {
          t2 = t2 ++ Array(freq(k)(j))
          if (freq(k)(j) > max) {
            max = freq(k)(j)
            }
          }
        tmax = tmax ++ Array(max)
        max = 0.0
        for (j<-20 to 39) {
          t3 = t3 ++ Array(freq(k)(j))
          if (freq(k)(j) > max) {
            max = freq(k)(j)
            }
          }
        tmax = tmax ++ Array(max)
        max = 0.0
        for (j<-40 to 79) {
          t4 = t4 ++ Array(freq(k)(j))
          if (freq(k)(j) > max) {
            max = freq(k)(j)
            }
          }
        tmax = tmax ++ Array(max)
        max = 0.0
        for (j<-80 to 159) {
          t5 = t5 ++ Array(freq(k)(j))
          if (freq(k)(j) > max) {
            max = freq(k)(j)
            }
          }
        tmax = tmax ++ Array(max)
        max = 0.0
        for (j<-160 to 511) {
          t6 = t6 ++ Array(freq(k)(j))
          if (freq(k)(j) > max) {
            max = freq(k)(j)
            }
          }
        tmax = tmax ++ Array(max)
        max = 0.0
      }
      var mean : Double = 0.0
      for (j<-0 to 5) {
        mean = mean + tmax(j)
      }
      mean = mean/6
      var tres : Array[Array[Double]] = Array(Array())
      for (k<-0 to freq.length){
        for (j<- 0 to freq(k).length) {
          if (freq(k)(j) > mean) {
            tres = tres ++ Array(Array(freq(k)(j),k+1))
          }
        }
      }
      var emp : Array[Array[Array[Double]]] = Array(Array(Array()))
      for (j<-0 to tres.length -6) {
        for (k<-3 to 7) {
          emp = emp ++ Array(Array(Array(tres(j)(0),tres(j+k)(0),tres(j+k)(1)-tres(j)(1)), Array(tres(j)(1)) ) )
        }
      }
    

    return emp
  }
    
  

  def max(l: Array[Double]): Double = {
    var m: Double = 0
    for (i <- 0 to l.length - 1) {
      if (l(i) > m) { m = l(i) }
    }
    return (m);
  }

  def module(C: complexes): Double = {
    var a = Seq(0)
    var b = sqrt(a(0) * (a(0)) + a(1) * (a(1)))
    return (b)
  }

}