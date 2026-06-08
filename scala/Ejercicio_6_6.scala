import scala.util.Try
object Ejercicio11 {

  // 1) Modelo de datos (lo que quieres producir)
  final case class Registro(id: Int, edad: Int, pais: String)

  // 2) Modelo de error (con contexto de negocio)
  sealed trait ErrorETL { def msg: String }

  final case class FormatoInvalido(msg: String) extends ErrorETL
  final case class CampoInvalido(campo: String, valor: String, motivo: String) extends ErrorETL {
    override def msg: String = s"Campo '$campo' inválido (valor='$valor'): $motivo"
  }

  // Helpers
  def parsearInt(campo: String, valor: String): Either[ErrorETL, Int] =
    Try(valor.toInt).toEither.left.map(_ => CampoInvalido(campo, valor, "no es un Int"))
  def validarNumero(campo: String, valor: String): Either[ErrorETL, Int] =
    if (valor.matches("^[0-9]{9}$")) Right(valor.toInt)
    else Left(CampoInvalido(campo, valor.toString, "debe ser un número de 9 dígitos"))

  def validarId(id: Int, original: String): Either[ErrorETL, Int] =
    if (id > 0) Right(id) else Left(CampoInvalido("id", original, "debe ser > 0"))

  def validarEdad(edad: Int, original: String): Either[ErrorETL, Int] =
    if (edad >= 0 && edad <= 120) Right(edad)
    else Left(CampoInvalido("edad", original, "debe estar entre 0 y 120"))

  def validarPais(pais: String): 
    //val paises = List("ES", "PT", "FR", "IT", "US")
    Either [ErrorETL, String] =
    if (List("ES", "PT", "FR", "IT", "US").contains(pais)) Right(pais)
    //Either[ErrorETL, String] =
    //if (pais.matches("^[A-Z]{2}$")) Right(pais)
    else Left(CampoInvalido("pais", pais, "debe ser un país reconocido, en formato de 2 letras"))

  // ✅ TU FUNCIÓN (Ejercicio)
  // Ahora devuelve ALL_ERRORS: si hay varios fallos en un mismo registro,
  // se recogen y se devuelven todos en una lista.
  def parsearRegistro(linea: String): Either[List[ErrorETL], Registro] = {
    val partes = linea.split(",").map(_.trim).toList

    partes match {
      case idS :: edadS :: paisS :: numeroS :: Nil =>
        // Validaciones independientes: cada una puede fallar y acumulamos errores.
        val idEither     = parsearInt("id", idS).flatMap(i => validarId(i, idS))
        val edadEither   = parsearInt("edad", edadS).flatMap(e => validarEdad(e, edadS))
        val paisEither   = validarPais(paisS)
        val numeroEither = validarNumero("numero", numeroS)

        // Recolectar todos los errores (cada Left contiene un ErrorETL)
        val errores: List[ErrorETL] = List(idEither, edadEither, paisEither, numeroEither)
          .collect { case Left(err) => err }

        if (errores.nonEmpty) Left(errores)
        else {
          // Todos son Right; extraer los valores con pattern matching seguro.
          val Right(id)     = idEither
          val Right(edad)   = edadEither
          val Right(pais)   = paisEither
          // el campo 'numero' no se usa en Registro pero lo validamos igualmente
          val Right(_)      = numeroEither
          Right(Registro(id, edad, pais))
        }

      case _ =>
        Left(List(FormatoInvalido(s"Se esperaban 4 campos 'id,edad,pais,numero' y llegaron ${partes.length}: $linea")))
    }
  }

  def main(args: Array[String]): Unit = {
    val ejemplos = List(
      "123,34,ES,664068114",    // válido
      "0,34,ES,66406811",       // id inválido
      "123,999,ES,664068114",    // edad inválida
      "123,34,España,664068114", // pais inválido
      "123,56,WX,664068114",    // pais inválido
      "abc,34,ES,664068114",     // id no int
      "123,xx,ES,664068114",     // edad no int
      "123,34,ES,664068114",         // formato inválido
      "123,34,ES, 66406811"         // formato inválido
      
    )

    ejemplos.foreach { linea =>
      val res = parsearRegistro(linea)

      // Consumir Either: fold (muy idiomático)
      // Ahora Left contiene una lista de errores; mostramos todos los mensajes.
      val salida = res.fold(
        errs => s" ERROR -> ${errs.map(_.msg).mkString("; ")}",
        ok   => s" OK    -> $ok"
      )

      println(f"$linea%-15s  $salida")
    }
  }
}

