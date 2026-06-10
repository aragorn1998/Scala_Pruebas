object Main {
  case class Record(key: String, value: Int)
  def main(args: Array[String]): Unit = {
    val data = Seq(
    Record("A", 1),
    Record("A", 7),
    Record("B", 3),
    Record("B", 4),
    Record("C", 18)
    )
    // Agrupar por el primer elemento de la tupla y contar las ocurrencias

    val result = data
      .groupBy(_.key)
      .map { case (k, v) => k -> v.size }
      .toList
      .sortBy(x => -x._2)

    //println(result)
    
    // Agrupar por el primer elemento de la tupla, sumar los valores del segundo elemento, ordenar por la suma y tomar los 2 primeros
    val result_2 = data
    .groupBy(_.key)
    .view
    .map { case (key, values) =>
    (key, values.map(_.value).sum)
    }
    .toList
    .sortBy(-_._2)
    .take(2)
    println(result_2)
  }
}
