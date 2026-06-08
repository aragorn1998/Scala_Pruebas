
val x = 10 
val y = 20

x + y

println("Hello Scala, welcome to the world of programming!")
val data = Seq(
    ("ES", 100, "casa"),
    ("FR", 200, "maison"),
    ("ES", 300, "casa"),
    ("ES", 150, "casa"),
    ("IT", 50, "casa")
)
println{data}
val result = data
  .filter(_._3 =="casa")
  .groupBy(_._1)
  .map { case (country, values) =>
    (country, values.map(_._2).sum, values.map(_._2).size, values.map(_._2).max, values.map(_._2).min)
  }
println(result) 

