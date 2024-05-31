import Datos._
import Itinerarios._

// Ejemp lo
val itsCurso = itinerarios(vuelosCurso,aeropuertosCurso)

/*
// 2 . 1 A e ropu e r tos incomun icados
val its1 = itsCurso("MID", "SVCS")
val its2 = itsCurso("CLO", "SVCS")
// 4 i t i n e r a r i o s CLO−SVO
val its3 = itsCurso("CLO", "SVO")
//2 i t i n e r a r i o s CLO−MEX
val its4 = itsCurso("CLO", "MEX")
//2 i t i n e r a r i o s CTG−PTY */
val its5 = itsCurso("CTG", "PTY")

val itSalidaCurso = itinerarioSalida(vuelosCurso,aeropuertosCurso)
val itsal1 = itSalidaCurso("CTG", "PTY", 11 , 50 )
//val itsal2 = itSalidaCurso("CTG", "PTY", 11 , 55 )
//val itsal3 = itSalidaCurso("CTG", "PTY", 10 , 30 )

