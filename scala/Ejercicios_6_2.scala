object ejercicio_1{
    def main(args:Array[String]){
        val edad = 30
        var contador = 1
        contador += 1
        println(s"Contador: $contador, Edad: $edad") 
    }
}

object ejercicio_2{
    def main(args:Array[String]){
        val numeros = 1 to 10
        val resultado = numeros.filter(_%2 == 0).map(_*10)
        println(s"Números: $numeros")
        println(s"Resultado: $resultado")
    }
}

object ejercicio_3{
        def esPar(num: Int): Boolean = {
            num % 2 == 0
        }    
        def main(args:Array[String]){

        val numeros = 1 to 10
        val resultado = numeros.filter(esPar)
        println(s"Números pares: $resultado")
    }
}

object ejercicio_4{
    def duplicar(num: Int): Int = {
        num * 2
    }
    def esPar(num: Int): Boolean = {
        num % 2 == 0
    }   
    def main(args:Array[String]){
    val numeros = 1 to 10
    val resultado = numeros.filter(esPar).map(duplicar)
    println(s"Resultado: $resultado")
    }
}

object ejercicio_5{
    def main(Args: Array[String]): Unit = {
        val frases = List(
            "Hola mundo",
            "Scala es genial",
            "Programar es divertido"
        )
        val resultado = frases.flatMap(_.toLowerCase.split(" "))
        println(s"Frases: $frases")
        println(s"Palabras: $resultado")
    }
}

object ejercicio_6{
    def dividir(num1: Double, num2: Double): Option[Double] = {
        if (num2 != 0) Some((num1 / num2)) else None
    }
    def main(args: Array[String]): Unit = {
        val resultado1 = dividir(9, 2)
        val resultado2 = dividir(10, 0)
        println(s"Resultado de dividir 9 entre 2: $resultado1")
        println(s"Resultado de dividir 10 entre 0: $resultado2")
    }
}

object ejercicio_7{
    def dividirSeguro(num1: Double, num2: Double): Option[Double] =
        if (num2 != 0) Some(num1 / num2) else None
    def main(args: Array[String]): Unit = {
        val resultado = dividirSeguro(10,0).flatMap(res => dividirSeguro(res, 5))
        println(s"Resultado de dividir 10 entre 2 y luego entre 5: $resultado")
    }

}

object ejercicio_8{
    def dividirSeguro(num1: Double, num2: Double): Option[Double] =
        if (num2 != 0) Some(num1 / num2) else None
    def main(args: Array[String]): Unit = {
        val resultado = for {
            res1 <- dividirSeguro(11, 2)
            res2 <- dividirSeguro(res1, 5)
        } yield res2
        println(s"Resultado de dividir 11 entre 2 y luego entre 5: $resultado")
    }
}

object ejercicio_9{
    import scala.util.{Try, Success, Failure}
    def dividirSeguro(num1: Double, num2: Double): Try[Double] =
        Try(num1 / num2)
    def parsear_int(str: String): Try[Int] =
        Try(str.toInt)
    def main(args: Array[String]): Unit = {
        val resultado = for{
            a<- parsear_int("9")
            b<- parsear_int("a")
            r <- dividirSeguro(a,b)
        } yield r
        
        resultado match {
            case Success(valor) =>
                println(s"Resultado correcto: $valor")
            case Failure(_) =>
                println("Error controlado")
            }

    }
}

import scala.util.Try

object Ejercicio10 {

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
  def parsearRegistro(linea: String): Either[ErrorETL, Registro] = {
    val partes = linea.split(",").map(_.trim).toList

    partes match {
      case idS :: edadS :: paisS :: Nil =>
        // Encadena validaciones con for-comprehension
        for {
          id   <- parsearInt("id", idS).flatMap(i => validarId(i, idS))
          edad <- parsearInt("edad", edadS).flatMap(e => validarEdad(e, edadS))
          pais <- validarPais(paisS)
        } yield Registro(id, edad, pais)

      case _ =>
        Left(FormatoInvalido(s"Se esperaban 3 campos 'id,edad,pais' y llegaron ${partes.length}: $linea"))
    }
  }

  def main(args: Array[String]): Unit = {
    val ejemplos = List(
      "123,34,ES",     // válido
      "0,34,ES",       // id inválido
      "123,999,ES",    // edad inválida
      "123,34,España", // pais inválido
      "123,56,WX",
      "abc,34,ES",     // id no int
      "123,xx,ES",     // edad no int
      "123,34"         // formato inválido
    )

    ejemplos.foreach { linea =>
      val res = parsearRegistro(linea)

      // Consumir Either: fold (muy idiomático)
      val salida = res.fold(
        err => s" ERROR -> ${err.msg}",
        ok  => s" OK    -> $ok"
      )

      println(f"$linea%-15s  $salida")
    }
  }
}

