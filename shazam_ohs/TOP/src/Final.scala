

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

  def moyenneStereo(wav2D: Array[Array[Int]]): Array[Array[Int]] = {
    var mono: Array[Array[Int]] = Array(Array())
    mono(0) = wav2D(0)
    if ((wav2D(0)(1) == 2)) {
      for (i <- 0 to wav2D(1).length - 1) {
        var temp: Int = (wav2D(1)(i) + wav2D(2)(i)) / 2
        mono(1) = mono(1) ++ Array(temp)
      }
      return mono
    } else {
      return wav2D
    }
  }

  def downsampling(tab: Array[Array[Int]]): Array[Array[Int]] = {
    var res: Array[Array[Int]] = Array(Array())
    res(0) = tab(0)
    var n: Int = tab.length / 4
    if (tab(0)(0) == 11025) {
      return tab
    }
    for (i <- 0 to n - 1) {
      var mean: Int = 0
      for (j <- 0 to 3) {
        mean = mean + tab(1)(n * 4 + j)
      }
      res(1) = res(1) ++ Array(mean / 4)
    }
    return res
  }

  def decoupe(tab: Array[Array[Int]]): Array[Array[Int]] = {
    var t: Array[Int] = tab(1)
    var compt: Int = 0
    var n: Int = t.length
    var ni: Int = n / 1024
    var res: Array[Array[Int]] = Array(Array())
    res(0) = tab(0)
    for (i <- 0 to (ni - 2)) {
      var temp: Array[Int] = Array()
      for (k <- 0 to 1024 - 1) {
        temp = temp ++ Array(t(i*1024 + k))
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

  def fftm(tab: Array[Array[Int]]): Array[Array[Double]] = {
    var n: Int = tab.length - 1
    var res: Array[Array[Double]] = Array(Array())
    res(0) = ItoD(tab(0))
    println("Veuillez patientez.. Etape 1 sur 2")
    for (i <- 1 to n) {
      res = res ++ Array(fftr(fft(ReToIm(tab(i)))))
    }
    println("Veuillez patientez.. Etape 2 sur 2")
    return res
  }

  def ItoD(tab: Array[Int]): Array[Double] = {
    var res: Array[Double] = Array()
    for (u <- 0 to tab.length - 1) {
      var temp: Double = tab(u)
      res = res ++ Array(temp)
    }
    return res
  }

  def BDDE(tab: Array[Array[Array[Int]]]): Array[Array[Array[Array[Double]]]] = {

    var bdde: Array[Array[Array[Array[Double]]]] = Array()
    var n : Int = tab.length
    var k : Int = 1
    for (i <- 0 to n-1) {
      println("Musique " + k + " sur " + n)
      bdde = bdde ++ Array(empreinte(tab(i)))
      k = k + 1
    }
    return bdde
  }

  def MtoC(tab: Array[Array[Double]]): Array[Array[Double]] = {
    var tabres: Array[Array[Double]] = Array(Array())
    tabres(0) = tab(0)
    var res: Array[Double] = Array()
    for (j <- 1 to tab.length - 1) {
      res = Array()
      for (i <- 0 to 511) {
        res = res ++ Array((tab(j)(2*i) + tab(j)(2*i + 1)) / 2)
      }
      tabres = tabres ++ Array(res)
    }
    return tabres
  }

  def empreinte(tab: Array[Array[Int]]): Array[Array[Array[Double]]] = {
    var t1: Array[Double] = Array()
    var t2: Array[Double] = Array()
    var t3: Array[Double] = Array()
    var t4: Array[Double] = Array()
    var t5: Array[Double] = Array()
    var t6: Array[Double] = Array()
    var test0 : Array[Array[Int]] = moyenneStereo(tab)
    var test1 : Array[Array[Int]] = downsampling(test0)
    var test2 : Array[Array[Int]] = decoupe(test1)
    var freqM: Array[Array[Double]] = fftm(test2)
    var freq: Array[Array[Double]] = MtoC(freqM)
    var fe: Double = tab(0)(0)
    var tmax: Array[Double] = Array()
    for (k <- 1 to freq.length - 1) {
      var max: Double = 0
      max = (freq(k).slice(0, 9).reduceLeft(_ max _))
      tmax = tmax ++ Array(max)
      max = (freq(k).slice(10, 19).reduceLeft(_ max _))
      tmax = tmax ++ Array(max)
      max = (freq(k).slice(20, 39).reduceLeft(_ max _))
      tmax = tmax ++ Array(max)
      max = (freq(k).slice(40, 79).reduceLeft(_ max _))
      tmax = tmax ++ Array(max)
      max = (freq(k).slice(80, 159).reduceLeft(_ max _))
      tmax = tmax ++ Array(max)
      max = (freq(k).slice(160, 511).reduceLeft(_ max _))
      tmax = tmax ++ Array(max)
    }
    var mean: Double = 0.0
    for (j <- 0 to 5) {
      mean = mean + tmax(j)
    }
    mean = mean / 6
    var tres: Array[Array[Double]] = Array()
    for (k <- 0 to freq.length - 1) {
    var c: Int = 0
      var j = -1
      while ( c < 10 && j < freq(k).length - 1) {
        j = j + 1
        if (freq(k)(j) > mean * 0.0) {
          tres = tres ++ Array(Array( j.toDouble , k + 1))
          c = c + 1
        }
      }
    }  
    var emp: Array[Array[Array[Double]]] = Array()
    for (j <- 0 to tres.length - 8) {
      for (k <- 3 to 7) {
        emp = emp ++ Array(Array(Array(tres(j)(0), tres(j + k)(0), tres(j + k)(1) - tres(j)(1)), Array(tres(j)(1))))
      }
    }
    return emp
  }

  def E_occur(E: Array[Array[Array[Double]]], M: Array[Array[Array[Double]]]): Array[Double] = {
    var T = new Array[Double](0)
    println("Recherche des occurences")
    for (k <- 0 to E.length - 5) {
          var test : Int  = 0
      for (c <- 0 to M.length - 5) {
        if ((E(k)(0).deep == M(c)(0).deep)) {
          //test = test + 1
        //}
       // if (E(k+1)(0).deep == M(c+1)(0).deep) {
         //test = test +1 
        //}
        //if (E(k+2)(0).deep == M(c+2)(0).deep) {
          //test = test + 1
        //}
        //if (E(k+3)(0).deep == M(c+3)(0).deep) {
          //test = test + 1
        //}
        //if (E(k+4)(0).deep == M(c+4)(0).deep) {
          //test = test + 1
        //}
        //if (test > 3) {
          T = T ++ Array(M(c)(1)(0) - E(k)(1)(0))
        }
        println( c + " sur " + M.length)
      }
    }
    return T
  }

  def memediff(T: Array[Double]): Int = {
    println("Calcul des similitudes")
    var res: Array[Int] = Array.fill(T.length)(0)
    for (i <- 0 to T.length - 1) {
      for (j <- 0 to T.length - 1) {
        if (T(i) == T(j)) { res(i) += 1 }
      }
    }
    return max(res)
  }

  def max(l: Array[Int]): Int = {
    var m: Int = 0
    for (i <- 0 to l.length - 1) {
      if (l(i) > m) {
        m = l(i)
      }
    }
    return (m);
  }

  def indice_max(l: Array[Int]): Int = {
    var m: Int = 0
    var res: Int = 0
    for (i <- 0 to l.length - 1) {
      if (l(i)> m) {
        m = l(i)
        res = i
      }
    }
    return (res);
  }
  def reconnaissance_m(wav2D: Array[Array[Int]], bdde: Array[Array[Array[Array[Double]]]]): String = {
    var tab_same: Array[Int] = Array()
    var E: Array[Array[Array[Double]]] = empreinte(wav2D)
    var M: Array[Array[Array[Double]]] = Array()
    for (i <- 0 to bdde.length - 1) {
      M = bdde(i)
      tab_same = tab_same ++ Array(memediff(E_occur(E, M)))
    }
    //println(E_occur(E,M).deep)
    println(tab_same.deep)
    var indice_musique = indice_max(tab_same)
    return (files(indice_musique))
  }

  def module(C: complexes): Double = {
    var a = Seq(0)
    var b = sqrt(a(0) * (a(0)) + a(1) * (a(1)))
    return (b)
  }

  def main(args: Array[String]): Unit = {
    var m1 = wav("The_Raiders_March_Mono-11025Hz.wav")
    println("Start")
    var bdde = BDDE(BDD)
    println("Next")
    println(reconnaissance_m(m1, bdde))
  }
}