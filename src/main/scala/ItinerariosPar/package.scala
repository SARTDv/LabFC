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
      val listaItinerarios = itinerariosPar(vuelos,aeropuertos)(cod1,cod2)

      def criterio(itinerario: Itinerario): Int = {
        itinerario.par.map(_.Esc).sum + itinerario.par.length - 1
      }
      listaItinerarios.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3)
    }
  }

  //Funcion 4.4
  def itinerariosAirePar(vuelos : List [ Vuelo ] , aeropuertos : List [ Aeropuerto] ) : ( String , String )=>List [Itinerario ]= {
    //Obtiene todos los itinerarios podibles
    val funcionItinerarioPar = itinerariosPar(vuelos, aeropuertos)
    //Calcula el tiempo total de un solo vuelo
    def funTiempoPar(vuelo:Vuelo):Int = {    
        val (a1, a2) = parallel(aeropuertos.par.filter(_.Cod == vuelo.Org), aeropuertos.par.filter(_.Cod == vuelo.Dst) )
        val (gmtSalida, gmtLlegada) = parallel((a1.head).GMT/100, (a2.head).GMT/100)
        val (horaSalida, horaLlegada) = parallel(( (vuelo.HS *60) + vuelo.MS) - gmtSalida, (vuelo.HL*60 + vuelo.ML) - gmtLlegada)
        val tiempoVuelo = horaLlegada - horaSalida
        if (tiempoVuelo <= 0) (24*60) + tiempoVuelo else tiempoVuelo
    }

    //Calcula el tiempo total de vuelo de un itinerario
    def calcularTiempoPar (v:Itinerario):Int = {
        val cadaTiempo =  v.par map (i => task(funTiempoPar(i)))
        (cadaTiempo map (l => l.join())).sum
    }

    //Funcion de salida, calcula los tres itinerarios que tienen menor tiempo en el aire
    def miItinerarioPar (aeropuerto1:String, aeropuerto2:String): List[Itinerario] = {
        val misItinerariosPar = funcionItinerarioPar(aeropuerto1, aeropuerto2)
        if (misItinerariosPar.length <= 3) misItinerariosPar
        else {
            val tiempos = misItinerariosPar map (c => (c, task(calcularTiempoPar(c))))
            val salida = tiempos.sortBy(_._2.join())
            (salida.unzip)._1.take(3)
        }
    }
    miItinerarioPar
  }

  //Funcion 4.5
  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    (cod1: String, cod2: String, h: Int, m: Int) => {
      val itinerario = itinerariosPar(vuelos,aeropuertos)(cod1,cod2)

      def calcularTiempoTotal(itinerario: Itinerario): Int = {
        val ultimoVuelo = task(itinerario.last)
        val minutosCita = task(h*60 + m)
        val minutosLlegada = task(ultimoVuelo.join.HL*60 + ultimoVuelo.join.ML)
        val tiempoAdicional = if (minutosCita.join >= minutosLlegada.join) minutosCita.join-minutosLlegada.join else {minutosCita.join-minutosLlegada.join + 24 * 60}
        tiempoAcumulado(itinerario, aeropuertos, tiempoAdicional)
      }

      itinerario.minByOption { listaVuelos => calcularTiempoTotal(listaVuelos) }
    }.getOrElse(List.empty)
  }
}
