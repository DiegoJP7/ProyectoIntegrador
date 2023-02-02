package Avance1

import com.github.tototoshi.csv._

import java.io.File
import play.api.libs.json.{JsArray, JsValue, Json}

object ColumnasJSON extends App {
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))
  val data = reader.allWithHeaders()

  reader.close()

  println("JSON \n")
  println("Ejemplo pequeño para el uso de play-json\n")
  val jsonString = """
{
  "name": "John",
  "age": 30,
  "city": "New York"
}
"""

  val json = Json.parse(jsonString)

  val name = (json \ "name").as[String]
  val age = (json \ "age").as[Int]
  val city = (json \ "city").as[String]

  println(jsonString+"\n")
  println("Nombre: "+name)
  println("Edad:" +age)
  println("Ciudad"+city)





  println("                                                       Lenguajes Hablados")
  val spoken_languages = data.flatMap(x => x.get("spoken_languages")).map(Json.parse).flatMap(_ \\ "name")
  val spoken_languagesV2 = data.flatMap(x => x.get("spoken_languages")).map(Json.parse).flatMap(_ \\ "iso_639_1")

  val lenguajesJSON = spoken_languages.groupBy(lenguajes => lenguajes)
    .map { case lenguajes => (lenguajes._1, lenguajes._2.size) }
  val prefixLenJSON=spoken_languagesV2.groupBy(prefix => prefix)
    .map{case prefix=>(prefix._1,prefix._2.size)}

  println("El lenguaje con mas peliculas es "+ lenguajesJSON.maxBy(x=>x._2)._1+" "+prefixLenJSON.maxBy(x=>x._2)._1+" con "+lenguajesJSON.maxBy(x=>x._2)._2)
  println("El lenguaje con menos  peliculas es "+ lenguajesJSON.minBy(x=>x._2)._1+prefixLenJSON.minBy(x=>x._2)._1+" con "+lenguajesJSON.minBy(x=>x._2)._2)
  println("---------------------------------------------------------------------")
  println("                                                       Companias Productoras")
  val production_companies = data.flatMap(x=>x.get("production_companies")).map(Json.parse).flatMap(_ \\ "name")
  val production_companiesV2 = data.flatMap(x=>x.get("production_companies")).map(Json.parse).flatMap(_ \\ "id")
  val companiasJSON = production_companies.groupBy
  {case companias => companias}
    .map{case companias=>(companias._1,companias._2.size)}
  val IDcomJSON= production_companiesV2.groupBy
  {case id=>id}
    .map{case id=>(id._1,id._2.size)}
  println("La compania Productora mas popular es "+companiasJSON.maxBy(x=>x._2)._1+" ID: "+IDcomJSON.maxBy(x=>x._2)._1+" con "+companiasJSON.maxBy(x=>x._2)._2+" peliculas")
  println("La compania Productora menos popular es "+companiasJSON.minBy(x=>x._2)._1+" ID: "+IDcomJSON.minBy(x=>x._2)._1+" con "+companiasJSON.minBy(x=>x._2)._2+" peliculas")

  println("---------------------------------------------------------------------")
  println("                                               Paises Productores")
  val production_countries = data.flatMap(x=>x.get("production_countries")).map(Json.parse).flatMap(_ \\ "name")
  val production_countriesV2 = data.flatMap(x=>x.get("production_countries")).map(Json.parse).flatMap(_ \\ "iso_3166_1")
  val paisesJSON=production_countries.groupBy
  { case paises=>paises}
    .map{case paises=>(paises._1,paises._2.size)}
  val prefixPaisesJSON=production_countriesV2.groupBy
  {case prefix=>prefix}
    .map{case prefix=>(prefix._1,prefix._2.size)}
  println("Pais que mas ha producido peliculas es "+paisesJSON.maxBy(x=>x._2)._1 +
    " con prefijo "+prefixPaisesJSON.maxBy(x=>x._2)._1+
    " con ID "+ +paisesJSON.maxBy(x=>x._2)._2)
  //
  println("Pais que mas ha producido peliculas es "+paisesJSON.minBy(x=>x._2)._1 +
    " con prefijo "+prefixPaisesJSON.minBy(x=>x._2)._1+
    " con ID "+ +paisesJSON.minBy(x=>x._2)._2)
}
