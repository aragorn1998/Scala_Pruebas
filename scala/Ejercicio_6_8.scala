object ejercicio_13{
    def main(args:Array[String]){
        val lst = List(1,2,3,4)
        println(lst.foldLeft(1.0)(_ / _))
    }
}
case class Venta(region: String, producto: String, cantidad: Int, precio: Double)
import org.apache.spark.sql.SparkSession

object ejercicio_14 {

  def main(args: Array[String]): Unit = {

    // Ejercicio 1
    val ventas = List(120, 45, 300, 88, 512, 30, 200)

    val p1 = ventas.filter(_ > 100)
    println(s"Ventas mayores a 100: $p1")

    val p2 = p1.map(_ * 0.9)
    println(s"Ventas con descuento del 10%: $p2")

    val p3 = p2.sum
    println(s"Total de ventas : $p3")

    // Ejercicio 2
    val paises = Map("ES" -> "España", "FR" -> "Francia", "DE" -> "Alemania")

    val italia = paises.get("IT").getOrElse("Desconocido")
    val espana = paises.get("ES").getOrElse("Desconocido")

    println(italia)

    // Ejercicio 3
    case class Empleado(nombre: String, departamento: String, salario: Double)

    val empleados = List(
      Empleado("Ana", "IT", 62000),
      Empleado("Luis", "Ventas", 38000),
      Empleado("Carmen", "HR", 27000),
      Empleado("Pedro", "IT", 51000)
    )

    empleados.foreach { e =>
      val cat = e.salario match {
        case s if s > 50000 => "Senior"
        case s if s >= 30000 => "Mid"
        case s if s < 30000 => "Junior"
      }
      println(s"${e.nombre} -> $cat")
    }

    // Ejercicio 4 (Spark)

    val spark = SparkSession.builder()
      .appName("Ejercicio_6_8_4")
      .master("local[*]")
      .getOrCreate()

    import spark.implicits._
    val datos = Seq(
      Venta("Norte", "Laptop", 5, 1200.0),
      Venta("Sur", "Mouse", 20, 25.0),
      Venta("Norte", "Mouse", 15, 25.0),
      Venta("Este", "Laptop", 3, 1200.0),
      Venta("Sur", "Laptop", 8, 1200.0)
    )
  
    val df = datos.toDF()

    df.show()

    spark.stop()
  }
}