# LabFC
Laboratorio funcional y concurrente 2024-I

## Informacion básica
**Nota: el código se basa en un 100% en scala, sin embargo debido a sbt puede fallar en ciertos sistemas operativos.**

El proyecto desarrollado en este repositorio tiene como objetivo encontrar itinerarios de vuelo para ciertas condiciones, los datos de vuelo se evidencian en el paquete "Datos"

En caso de que el usuario final desee usar sus propios datos debe tener en cuenta las siguientes reglas para definir aeropuertos y vuelos:

`Aeropuerto(Cod: String, X: Int, Y: Int, GMT: Int)` 

Para Aeropuerto se tiene un código que refiere al código único del aeropuerto, dos numeros enteros que refieren a las coordenadas del aeropuerto y por ultimo un gmt en multiplos de 100 que refiere a la franja horaria.

`Vuelo(Aln: String, Num: Int, Org: String, HS: Int, MS: Int, Dst: String, HL: Int, ML: Int, Esc: Int)` 

Para Vuelo se tiene Aerolinea, número de vuelo, hora y minutos de vuelo en gmt origen, destino, hora y minutos del vuelo en gmt destino, por último el número de escalas realizadas por dicho vuelo.

### Descripcion de funciones

Antes de iniciar con la ejecución estás son las funciones y una breve descripción de las mismas:

`itinerarios` Encuentra todas las maneras de llegar de un aeropuerto a otro.

`itinerariosTiempo` Escoje los primeros 3 itinerarios con el tiempo total más bajo.

`itinerariosEscalas` Escoje los primeros 3 itinerarios con el menor número de escalas.

`itinerariosAire` Escoje los primeros 3 itinerarios con el menor tiempo en aire.

`itinerarioSalida` En caso de que se desee llegar a una cita a tiempo, pero se busca salir tan tarde como sea posible.

## Ejecucion de las funciones

### Pasos importantes
Una vez el paquete de Datos esté listo (Tiene datos iniciales si así lo desea) se debe seguir ciertos pasos para dar inicio al uso de las funciones propuestas.
En el directorio en el que se encuentra el archivo "sbt" posterior al clonado de este repositorio se ejecutan los siguientes comandos:

```sbt``` 

Este Permite iniciar el compilador a base de ciertos requerimientos

```console```

Permite ingresar a la consola de scala

```
import Datos._
import Itinerarios._
import ItinerariosPar._
import Benchmark._
```

Los anteriores comandos hacen que los paquetes sean considerados para llevar a cabo las funciones dentro de los mismos

### Ejecucion

Para las funciones excepto itinerarioSalida se debe invocar de la siguiente manera:
_funcion_(vuelos,aeropuertos)(códigoOrigen,códigoDestino)

En donde: 

- vuelos es la lista de vuelos para los que se desea calcular itinerarios, este valor se encuentra dentro del paquete Datos
- aeropuertos, lista de aeropuertos encontrada en el paquete Datos
- CodigoOrigen, es una cadena de caracteres que se refiere al código del aeropuerto de origen
- CodigoDestino, cadena que refiere al aeropuerto destino

Para la función itinerario salida:
_itinerarioSalida_(vuelos,aeropuertos)(codigoOrigen,codigoDestino,h,m)

En donde: 

- Vuelos,aeropuertos se refiere a las listas encontradas en Datos
- CodigoOrigen, codigoDestino refiere a los códigos de los aeropuertos de inicio y destino respectivamente
- H, hora de la cita con gmt del aeropuerto destino
- M, minutos de la cita con gmt del aeropuerto de destino

Cada función cuenta con una versión en la que se usa paralelizacion de tareas y datos, esta función se usa exactamente igual que su versión secuencial con la diferencia de que a su nombre de añade "Par":

*funcion*Par(...)(...)

Estas funciones deben usarse con cuidado, puesto que para una cantidad de datos pequeña el costo y tiempo de ejecución son mayores a su versión secuencial.

## Velocidad de ejecucion

Por último, es posible analizar la diferencia de velocidad entre dos búsquedas de itinerarios para los mismos datos, esto se puede hacer de la siguiente manera:

### Para todas a excepcion de _itinerarioSalida_
_compararAlgoritmos_(a1:AlgoritmoItinerarios, a2:AlgoritmoItinerarios)
                    (vuelos:List[Vuelo], aeropuertos:List[Aeropuerto])
                    (cod1: String, cod2: String)

En donde: 

- a1 y a2 son las funciones que se desean analizar
- vuelos,aeropuertos se refieren a las listas encontradas en Datos
- cod1,cod2 se refieren a los códigos de aeropuertos origen y destino respectivamente

### Para _itinerarioSalida_

_compararAlgoritmosSalida_(a1:AlgoritmoItinerarioSalida, a2:AlgoritmoItinerarioSalida)
                          (vuelos:List[Vuelo], aeropuertos:List[Aeropuerto])
                          (cod1: String, cod2: String, h:Int, m:Int )
                          
En donde: 

- a1 y a2 son las funciones que se desean analizar
- vuelos,aeropuertos se refieren a las listas encontradas en Datos
- cod1,cod2 se refieren a los códigos de aeropuertos origen y destino respectivamente
- h,m hora y minutos de la cita en gmt de destino

### Resultados
Los resultados de aplicar estas dos funciones son:

(TiempoPrimera,TiempoSegunda,aceleración)

- TiempoPrimera se refiere al tiempo que de tardó la primera función para los argumentos dados
- TiempoSegunda tiempo que se tarda la segunda función con los argumentos dados
- Aceleración, refiere a aceleración de la segunda función con respecto a la primera, si esta es menor a 1 significa que la segunda función se tarda más que la primera
