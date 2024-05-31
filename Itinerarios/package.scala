import Datos._

package object Itinerarios {
  type Itinerario = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    //Funcion para encontrar los itinerarios, toma dos codigos, codigo1 el origen y codigo2 la llegada
    def encontrarItinerarios(codigo1: String, codigo2: String): List[Itinerario] = {


      // Función auxiliar para encontrar los vuelos que parten de un aeropuerto con el codigo dado
      def vuelosDesde(codigo: String): List[Vuelo] =
        vuelos.filter(v => v.Org == codigo)


      // Función auxiliar para encontrar los itinerarios de un vuelo hasta el destino de manera recursiva
      def itinerariosDesdeVuelo(vuelo: Vuelo, destino: String,aeropuertosVisitados: Set[String]): List[Itinerario] =
        if (vuelo.Dst == destino)
          //Si el aeropuerto de destino del vuelo coincide con el destino buscado, devuelve una lista con un solo
          // itinerario que contiene ese vuelo
          List(List(vuelo))
        else if (!aeropuertosVisitados.contains(vuelo.Dst))
          /* De lo contrario, busca todos los vuelos que parten desde el aeropuerto del destino actual
           * solamente cuando el set de aeropuertos visitados no contenga el destino del vuelo
           * y para cada uno de ellos calcula los itinerarios que parten desde ese vuelo hasta el destino buscado */
          vuelosDesde(vuelo.Dst).flatMap(v => itinerariosDesdeVuelo(v, destino,aeropuertosVisitados + vuelo.Dst).map(vuelo :: _))
        else List()
      // Con el flatMap concatenamos todos los itinerarios encotrados y map para agregar el vuelo actual al incicio
      //de cada uno de estos itinerarios


      /* lógica principal, obtiene todos los vuelos que parten desde el aeropuerto de origen y para cada uno de ellos, calcula
       * los itinerarios hasta el aeropuerto de destino, se hace una comprobacion de validez para asegurarse que
       * el vuelo en el destino no salga antes de que llegue el vuelo abordado */
      vuelosDesde(codigo1).flatMap(v => itinerariosDesdeVuelo(v, codigo2,Set(codigo1)))
    }

    encontrarItinerarios
  }
  

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    def gmtDeAeropuerto(codigo: String): Int = aeropuertos.find(_.Cod == codigo).map(_.GMT).getOrElse(0)

    def minutosReales(h:Int,m:Int,gmt:Int):Int = {
      val hTotal = h - gmt
      if (hTotal > 24 ) (hTotal-24)*60 + m else hTotal*60 + m
    }

    // Función auxiliar para calcular la diferencia en minutos entre dos horas (considerando los minutos)
    def calcularDiferenciaMinutos(hs:Int,ms:Int,gmts:Int,hl:Int,ml:Int,gmtl:Int): Int = {
      minutosReales(hs,ms,gmts)-minutosReales(hl,ml,gmtl)
    }

    (cod1: String, cod2: String, h: Int, m: Int) => {
      val itinerario = itinerarios(vuelos,aeropuertos)(cod1,cod2)

        def calcularTiempoTotal(itinerario: Itinerario): Int = {

          def tiempoAcumulado(vuelos: Itinerario, tiempoActual: Int): Int = vuelos match {
            case vuelo :: Nil =>
              // Ultimo vuelo, solo sumar el tiempo de vuelo si el tiempo de vuelo es negativo, se añade un dia
              val tiempoVuelo = calcularDiferenciaMinutos(vuelo.HL, vuelo.ML,gmtDeAeropuerto(vuelo.Dst),
                                                          vuelo.HS, vuelo.MS, gmtDeAeropuerto(vuelo.Org))

              if (tiempoVuelo <= 0 ) {24*60 + tiempoVuelo} else {tiempoActual + tiempoVuelo}

            case vuelo :: siguienteVuelo :: resto =>

              val tiempoVueloSinAjuste = calcularDiferenciaMinutos(vuelo.HL, vuelo.ML,gmtDeAeropuerto(vuelo.Dst),
                                                                   vuelo.HS, vuelo.MS, gmtDeAeropuerto(vuelo.Org))

              val tiempoVuelo = if (tiempoVueloSinAjuste < 0 ) { (24*60) + tiempoVueloSinAjuste} else {tiempoActual + tiempoVueloSinAjuste}

              val tiempoEspera = calcularDiferenciaMinutos(siguienteVuelo.HS, siguienteVuelo.MS, gmtDeAeropuerto(siguienteVuelo.Org),
                                                           vuelo.HL, vuelo.ML, gmtDeAeropuerto(vuelo.Dst))

              val tiempoAjustado = if (tiempoEspera < 0) {
              // Si la hora de llegada es mayor que la hora de salida del próximo vuelo, agregar un día
                tiempoActual + tiempoVuelo + (24 * 60 + tiempoEspera)
              } else {
                tiempoActual + tiempoVuelo + tiempoEspera
              }

            tiempoAcumulado(siguienteVuelo :: resto, tiempoAjustado)
          }

        val ultimoVuelo = itinerario.last
        val minutosCita = h*60 + m
        val minutosLlegada = ultimoVuelo.HL*60 + ultimoVuelo.ML
        val tiempoAdicional = if (minutosCita >= minutosLlegada) minutosCita-minutosLlegada else {minutosCita-minutosLlegada + 24 * 60}
        tiempoAcumulado(itinerario, tiempoAdicional)
      }

      itinerario.minByOption { listaVuelos => calcularTiempoTotal(listaVuelos) }
      }.getOrElse(List.empty)
  }
}
