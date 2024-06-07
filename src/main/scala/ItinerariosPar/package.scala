import common._
import Datos._
import scala.collection.parallel.immutable._ //Para ParSeq
import scala.collection.parallel.CollectionConverters.*  //Se debe importar para .par

package object ItinerariosPar {

  type Itinerario = List[Vuelo]
  

  def gmtDeAeropuertoPar(aeropuertos: List[Aeropuerto],codigo: String): Int = aeropuertos.par.find(_.Cod == codigo).map(_.GMT).getOrElse(0)

  def minutosReales(h:Int,m:Int,gmt:Int):Int = {
    val hTotal = h - (gmt/100)
    if (hTotal >= 24 ) (hTotal-24)*60 + m else hTotal*60 + m
  }

  // Función auxiliar para calcular la diferencia en minutos entre dos horas (considerando los minutos)
  def calcularDiferenciaMinutosPar(hs:Int,ms:Int,gmts:Int,hl:Int,ml:Int,gmtl:Int): Int = {

    if (gmts==gmtl) {
        val (cal1, cal2) = parallel((hs*60+ms),(hl*60+ml))
        cal1 - cal2
    } else {  
        val  (cal3, cal4) = parallel(minutosReales(hs,ms,gmts),minutosReales(hl,ml,gmtl))
        cal3 - cal4
    }
  }

  def tiempoAcumulado(vuelos: Itinerario,aeropuertos: List[Aeropuerto], tiempoActual: Int): Int = vuelos match {
    case Nil => 0
    case vuelo :: Nil =>
      // Ultimo vuelo, solo sumar el tiempo de vuelo si el tiempo de vuelo es negativo, se añade un dia
      val (gmtFormateadoDst, gmtFormateadoOrg) = parallel(gmtDeAeropuertoPar(aeropuertos,vuelo.Dst), gmtDeAeropuertoPar(aeropuertos,vuelo.Org))
      val tiempoVuelo = calcularDiferenciaMinutosPar(vuelo.HL, vuelo.ML,gmtFormateadoDst,
                                                  vuelo.HS, vuelo.MS, gmtFormateadoOrg)
                                            
      if (tiempoVuelo <= 0 ) {tiempoActual + 24*60 + tiempoVuelo } else {tiempoActual + tiempoVuelo}

    case vuelo :: siguienteVuelo :: resto =>
      val (gmtDstFormateado, gmtOrgFormateado) = parallel(gmtDeAeropuertoPar(aeropuertos,vuelo.Dst), gmtDeAeropuertoPar(aeropuertos,vuelo.Org))
      val tiempoVueloSinAjuste = calcularDiferenciaMinutosPar(vuelo.HL, vuelo.ML,gmtDstFormateado,
                                                            vuelo.HS, vuelo.MS, gmtOrgFormateado)

      val tiempoVuelo = if (tiempoVueloSinAjuste <= 0 ) { (24*60) + tiempoVueloSinAjuste} else {tiempoVueloSinAjuste}

      //gmt deben ser iguales
      val tiempoEspera = calcularDiferenciaMinutosPar(siguienteVuelo.HS, siguienteVuelo.MS, gmtDeAeropuertoPar(aeropuertos,siguienteVuelo.Org),
                                                    vuelo.HL, vuelo.ML, gmtDstFormateado)

      val tiempoAjustado = if (tiempoEspera < 0) {
        // Si la hora de llegada es mayor que la hora de salida del próximo vuelo, agregar un día
        tiempoActual + tiempoVuelo + (24 * 60 + tiempoEspera)
      } else {
        tiempoActual + tiempoVuelo + tiempoEspera
      }

      tiempoAcumulado(siguienteVuelo :: resto,aeropuertos, tiempoAjustado)
  }


  //Funcion 4.1
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def TodosLosItinerarios(origen: String, destino: String, aeropuertosVisitados: Set[String]): List[Itinerario] = {
      if (origen == destino) { return List(List.empty) }

      val vuelosT = for {
        vuelo <- vuelos.filter(_.Org == origen)
        if (!aeropuertosVisitados.contains(vuelo.Dst))
      } yield task(TodosLosItinerarios(vuelo.Dst, destino, aeropuertosVisitados + vuelo.Dst).map(vuelo :: _))

      vuelosT.flatMap(vuelos => vuelos.join)
      }

    (cod1: String, cod2: String) => {
      TodosLosItinerarios(cod1, cod2, Set(cod1))
    }
  }

  //Funcion 4.2
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1: String, cod2: String) => {
      val listaItinerarios = itinerariosPar(vuelos,aeropuertos)(cod1,cod2)
      val listaParela = listaItinerarios map (it => (it, task(tiempoAcumulado(it, aeropuertos, 0))))
      listaParela.sortBy(_._2.join).unzip._1.take(3)
    }
  }

  //Funcion 4.3
  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    (cod1: String, cod2: String) => {
      def criterio(itinerario: Itinerario): Int = {
        itinerario.map(_.Esc).sum + itinerario.length - 1
      }

      val listaItinerarios = itinerariosPar(vuelos,aeropuertos)(cod1,cod2)
      val longitud: Int = listaItinerarios.length
      if (longitud < 8) listaItinerarios.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3)
      else {
        val l1: Int = longitud / 4
        val l2: Int = longitud * 2 / 4
        val l3: Int = longitud * 3 / 4

        val (it1,it2,it3,it4) = parallel(
          listaItinerarios.slice(0, l1),
          listaItinerarios.slice(l1, l2),
          listaItinerarios.slice(l2, l3),
          listaItinerarios.slice(l3, longitud)
        )
        val (m1,m2,m3,m4) = parallel(
          it1.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3),
          it2.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3),
          it3.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3),
          it4.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3)
        )

        (m1++m2++m3++m4).sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3)
      }
    }
  }


  //Funcion 4.4
  def itinerariosAirePar(vuelos : List [ Vuelo ] , aeropuertos : List [ Aeropuerto] ) : ( String , String )=>List [Itinerario ]= {
    //Obtiene todos los itinerarios podibles
    val funcionItinerarioPar = itinerariosPar(vuelos, aeropuertos)
    //Calcula el tiempo total de un solo vuelo
    def funTiempoPar(vuelo:Vuelo):Int = {    
        val a1 = aeropuertos.filter(_.Cod == vuelo.Org)
        val a2 = aeropuertos.filter(_.Cod == vuelo.Dst)
        val gmtSalida = (a1.head).GMT/100
        val gmtLlegada = (a2.head).GMT/100
        val horaSalida = ((vuelo.HS*60) + vuelo.MS) - gmtSalida
        val horaLlegada = ((vuelo.HL*60) + vuelo.ML) - gmtLlegada
        val tiempoVuelo = horaLlegada - horaSalida
        if (tiempoVuelo <= 0) (24*60) + tiempoVuelo else tiempoVuelo
    }

    //Calcula el tiempo total de vuelo de un itinerario
    def calcularTiempoPar (v:Itinerario):Int = {
      (v.par map (i => funTiempoPar(i))).sum
    }

    //Funcion de salida, calcula los tres itinerarios que tienen menor tiempo en el aire
    def miItinerarioPar (aeropuerto1:String, aeropuerto2:String): List[Itinerario] = {
        val misItinerariosPar = funcionItinerarioPar(aeropuerto1, aeropuerto2)
        if (misItinerariosPar.length <= 3) misItinerariosPar
        else {
            val tiempos = misItinerariosPar map (c => (c, task(calcularTiempoPar(c))))
            val salida = tiempos.sortBy(_._2.join)
            (salida.unzip)._1.take(3)
        }
    }
    miItinerarioPar
  }

  //Funcion 4.5
  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    (cod1: String, cod2: String, h: Int, m: Int) => {

      def calcularTiempoTotal(itinerario: Itinerario): Int = {
        val ultimoVuelo = itinerario.last
        val minutosCita = h*60 + m
        val minutosLlegada = ultimoVuelo.HL*60 + ultimoVuelo.ML
        val tiempoAdicional = if (minutosCita >= minutosLlegada) minutosCita-minutosLlegada else {minutosCita-minutosLlegada + 24 * 60}
        tiempoAcumulado(itinerario, aeropuertos, tiempoAdicional)
      }

      val itinerario = itinerariosPar(vuelos,aeropuertos)(cod1,cod2)
      val longitud: Int = itinerario.length
      if (longitud < 8) itinerario.minByOption { listaVuelos => calcularTiempoTotal(listaVuelos) }.getOrElse(List.empty)
      else {
        val l1: Int = longitud / 4
        val l2: Int = longitud * 2 / 4
        val l3: Int = longitud * 3 / 4

        val (it1,it2,it3,it4) = parallel(
          itinerario.slice(0, l1),
          itinerario.slice(l1, l2),
          itinerario.slice(l2, l3),
          itinerario.slice(l3, longitud)
        )
        val (m1,m2,m3,m4) = parallel(
          it1.map(listaVuelos => (listaVuelos, calcularTiempoTotal(listaVuelos))).minByOption(_._2).getOrElse((List.empty[Vuelo], 0)),
          it2.map(listaVuelos => (listaVuelos, calcularTiempoTotal(listaVuelos))).minByOption(_._2).getOrElse((List.empty[Vuelo], 0)),
          it3.map(listaVuelos => (listaVuelos, calcularTiempoTotal(listaVuelos))).minByOption(_._2).getOrElse((List.empty[Vuelo], 0)),
          it4.map(listaVuelos => (listaVuelos, calcularTiempoTotal(listaVuelos))).minByOption(_._2).getOrElse((List.empty[Vuelo], 0))
        )

        List(m1,m2,m3,m4).minBy(_._2)._1
      }
    }
  }
}