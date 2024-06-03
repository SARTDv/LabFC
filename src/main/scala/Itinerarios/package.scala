import Datos._

package object Itinerarios {
  type Itinerario = List[Vuelo]

  def gmtDeAeropuerto(aeropuertos: List[Aeropuerto],codigo: String): Int = aeropuertos.find(_.Cod == codigo).map(_.GMT).getOrElse(0)

  def minutosReales(h:Int,m:Int,gmt:Int):Int = {
    val hTotal = h - (gmt/100)
    if (hTotal >= 24 ) (hTotal-24)*60 + m else hTotal*60 + m
  }

  // Función auxiliar para calcular la diferencia en minutos entre dos horas (considerando los minutos)
  def calcularDiferenciaMinutos(hs:Int,ms:Int,gmts:Int,hl:Int,ml:Int,gmtl:Int): Int = {
    if (gmts==gmtl) {(hs*60+ms) - (hl*60+ml)} else  minutosReales(hs,ms,gmts) - minutosReales(hl,ml,gmtl)
  }

  def tiempoAcumulado(vuelos: Itinerario,aeropuertos: List[Aeropuerto], tiempoActual: Int): Int = vuelos match {
    case vuelo :: Nil =>
      // Ultimo vuelo, solo sumar el tiempo de vuelo si el tiempo de vuelo es negativo, se añade un dia
      val tiempoVuelo = calcularDiferenciaMinutos(vuelo.HL, vuelo.ML,gmtDeAeropuerto(aeropuertos,vuelo.Dst),
                                                  vuelo.HS, vuelo.MS, gmtDeAeropuerto(aeropuertos,vuelo.Org))

      if (tiempoVuelo <= 0 ) {tiempoActual + 24*60 + tiempoVuelo } else {tiempoActual + tiempoVuelo}

    case vuelo :: siguienteVuelo :: resto =>

      val tiempoVueloSinAjuste = calcularDiferenciaMinutos(vuelo.HL, vuelo.ML,gmtDeAeropuerto(aeropuertos,vuelo.Dst),
                                                            vuelo.HS, vuelo.MS, gmtDeAeropuerto(aeropuertos,vuelo.Org))

      val tiempoVuelo = if (tiempoVueloSinAjuste <= 0 ) { (24*60) + tiempoVueloSinAjuste} else {tiempoVueloSinAjuste}

      //gmt deben ser iguales
      val tiempoEspera = calcularDiferenciaMinutos(siguienteVuelo.HS, siguienteVuelo.MS, gmtDeAeropuerto(aeropuertos,siguienteVuelo.Org),
                                                    vuelo.HL, vuelo.ML, gmtDeAeropuerto(aeropuertos,vuelo.Dst))

      val tiempoAjustado = if (tiempoEspera < 0) {
        // Si la hora de llegada es mayor que la hora de salida del próximo vuelo, agregar un día
        tiempoActual + tiempoVuelo + (24 * 60 + tiempoEspera)
      } else {
        tiempoActual + tiempoVuelo + tiempoEspera
      }

      tiempoAcumulado(siguienteVuelo :: resto,aeropuertos, tiempoAjustado)
  }


  //Funcion 3.1
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def TodosLosItinerarios(origen: String, destino: String, aeropuertosVisitados: Set[String]): List[Itinerario] = {
      if (origen == destino) { return List(List.empty) }

      vuelos.filter(_.Org == origen).flatMap { vuelo =>
        if (!aeropuertosVisitados.contains(vuelo.Dst)) {
          val subItinerarios = TodosLosItinerarios(vuelo.Dst, destino,  aeropuertosVisitados + vuelo.Dst)
          subItinerarios.map(vuelo :: _ )
        } else {
          List()
        }
      }
    }

    (cod1: String, cod2: String) => {
      TodosLosItinerarios(cod1, cod2, Set(cod1))
    }
  }

  //Funcion 3.2
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1: String, cod2: String) => {
      val listaItinerarios = itinerarios(vuelos,aeropuertos)(cod1,cod2)
      listaItinerarios.sortBy(itinerario => tiempoAcumulado(itinerario, aeropuertos, 0)).take(3)
    }
  }

  //Funcion 3.3
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    (cod1: String, cod2: String) => {
      val listaItinerarios = itinerarios(vuelos,aeropuertos)(cod1,cod2)

      def criterio(itinerario: Itinerario): Int = {
        itinerario.map(_.Esc).sum + itinerario.length - 1
      }
      listaItinerarios.sortWith((it1, it2) => criterio(it1) < criterio(it2)).take(3)
    }
  }

  //Funcion 3.4
  def itinerariosAire(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String) => List[Itinerario] = {
    //Obtiene todos los itinerarios posibles
    val funcionItinerario = itinerarios(vuelos, aeropuertos)
    //Calcula el tiempo total de un solo vuelo
    def funTiempo(vuelo:Vuelo):Int = {
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
    def calcularTiempo (v:Itinerario):Int = {
      (v map (i => funTiempo(i))).sum
    }
    //Funcion de salida, calcula los tres itinerarios que tienen menor tiempo en el aire
    def miItinerario (aeropuerto1:String, aeropuerto2:String): List[Itinerario] = {
      val misItinerarios = funcionItinerario(aeropuerto1, aeropuerto2)
      if (misItinerarios.length <= 3) misItinerarios 
      else {
        val tiempos = misItinerarios map (c => (c, calcularTiempo(c)))
        val salida = tiempos.sortBy(_._2)
        (((salida.unzip)._1).toList).take(3)
      }
    }
    miItinerario
  }

  //Funcion 3.5
  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    (cod1: String, cod2: String, h: Int, m: Int) => {
      val itinerario = itinerarios(vuelos,aeropuertos)(cod1,cod2)

      def calcularTiempoTotal(itinerario: Itinerario): Int = {
        val ultimoVuelo = itinerario.last
        val minutosCita = h*60 + m
        val minutosLlegada = ultimoVuelo.HL*60 + ultimoVuelo.ML
        val tiempoAdicional = if (minutosCita >= minutosLlegada) minutosCita-minutosLlegada else {minutosCita-minutosLlegada + 24 * 60}
        tiempoAcumulado(itinerario, aeropuertos, tiempoAdicional)
      }

      itinerario.minByOption { listaVuelos => calcularTiempoTotal(listaVuelos) }
      }.getOrElse(List.empty)
  }
}
