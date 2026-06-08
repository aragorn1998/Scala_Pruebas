import scala.io.Source

object ReadCSV {
  def main(args: Array[String]): Unit = {

    val file = Source.fromFile("c:\\Users\\a.i.garcia\\Downloads\\dataframe_ejemplo.csv")

    val data = file.getLines().drop(1).map { line =>
      val cols = line.split(",").map(_.trim)
      (cols(0).toInt, cols(1), cols(2), cols(3).toDouble)
    }.toList

    file.close()
    case class Record(id: Int, country: String, category: String, value: Double)
    val result = data.map { case (id, country, category, value) => Record(id, country, category, value) }
    case class Record_2(country: String, suma: Double, count: Int, avg: Double)
    val results = result
    .groupBy(_.country)
      .view
      .map{
        case (country, values) =>
        val suma = values.map(_.value).sum
        val count = values.size
        val avg = if(count > 0) suma / count else 0
        Record_2(country,suma, count, avg)
      }
      .toList
      .sortBy(-_.suma)
    println(results)
}
}
