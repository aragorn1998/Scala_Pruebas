object Ejercicio_basic {
    case class Record(key: String, value: Int, country: String)
    def main(args: Array[String]): Unit = {
        val data = Seq(
        Record("u1", 100, "ES"),
        Record("u1", 200, "ES"),
        Record("u2", 50, "FR"),
        Record("u2", 150, "FR"),
        Record("u3", 300, "ES"),
        Record("u3", 100, "ES"),
        Record("u4", 400, "UK")
        )
    case class UserAgg(key: String, count: Int, sum: Int, avg: Double)    
    val result = data
        .groupBy(_.key)
        .view
        .map{
            case (key, values) =>
                val suma = values.map(_.value).sum
                val count = values.map(_.value).size
                val avg = if(count > 0) suma.toDouble / count else 0
            UserAgg(key, count, suma, avg)
        }
        .toList
        .sortBy(-_.avg)
        .take(2)
    println(result)
    }
}