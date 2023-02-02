package Avance1

import com.cibo.evilplot.plot.{BarChart, Histogram, PieChart}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme.{DefaultElements, DefaultTheme}
import com.github.tototoshi.csv._

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.util.matching.Regex
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



  implicit val theme = DefaultTheme.copy(
    elements = DefaultElements.copy(categoricalXAxisLabelOrientation = 45)
  )

  println("                                                       Lenguajes Hablados")
  val spoken_languages = data.flatMap(x => x.get("spoken_languages")).map(Json.parse).flatMap(_ \\ "name")
  val spoken_languagesV2 = data.flatMap(x => x.get("spoken_languages")).map(Json.parse).flatMap(_ \\ "iso_639_1")

  val spokenGroupBy=spoken_languages.groupBy{
    case x => x
  }.map {
    case x => (x._1, x._2.size)
  }.toList.sortBy(_._2).reverse

  val lenguajesJSON = spoken_languages.groupBy(lenguajes => lenguajes)
    .map { case lenguajes => (lenguajes._1, lenguajes._2.size) }
  val prefixLenJSON=spoken_languagesV2.groupBy(prefix => prefix)
    .map{case prefix=>(prefix._1,prefix._2.size)}

  println("El lenguaje con mas peliculas es "+ lenguajesJSON.maxBy(x=>x._2)._1+" "+prefixLenJSON.maxBy(x=>x._2)._1+" con "+lenguajesJSON.maxBy(x=>x._2)._2)
  println("El lenguaje con menos  peliculas es "+ lenguajesJSON.minBy(x=>x._2)._1+prefixLenJSON.minBy(x=>x._2)._1+" con "+lenguajesJSON.minBy(x=>x._2)._2)

  val top5spken= spokenGroupBy.take(5)

  BarChart(top5spken.map(_._2))
    .title("Top 5 Lenguajes Hablados")
    .xAxis(top5spken.map(_._1.toString()))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/DatosNoNumericos/Top5LenguajesHablados.png"))

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

  val comGroupBy=production_companies.groupBy{
    case x => x
  }.map {
    case x => (x._1, x._2.size)
  }.toList.sortBy(_._2).reverse

  val top5com=comGroupBy.take(5)

  BarChart(top5com.map(_._2))
    .title("Top 5 Compañias Productoras")
    .xAxis(top5com.map(_._1.toString()))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/DatosNoNumericos/Top5Companias.png"))

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

  val countriesGroupBy=production_countries.groupBy{
    case x => x
  }.map {
    case x => (x._1, x._2.size)
  }.toList.sortBy(_._2).reverse

  val top5countries=countriesGroupBy.take(5)

  BarChart(top5countries.map(_._2))
    .title("Top 5 Paises Productores")
    .xAxis(top5countries.map(_._1.toString()))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/DatosNoNumericos/Top5PaisesP.png"))

  //Fechas
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  val releaseDateList = data
    .map(row => row("release_date"))
    .filter(!_.equals(""))
    .map(text => LocalDate.parse(text, dateFormatter))

  //val yearReleaseList = releaseDateList.map(_.getYear)

  val yearReleaseList = releaseDateList
    .map(_.getYear)
    .map(_.toDouble)

  printf("\nAño menor: %f", yearReleaseList.min)
  printf("\nAño mayor: %f\n", yearReleaseList.max)

  Histogram(yearReleaseList)
    .title("Años de lanzamiento")
    .xAxis()
    .yAxis()
    .xbounds(1916.0, 2018.0)
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/DatosNoNumericos/Years.png"))

  //Taller
  def funcion(original: String, patternsTR: List[(Regex, String, String)]) = {

    var txtOr = original
    for (p <- patternsTR) {
      for (m <- p._1.findAllIn(txtOr)) {
        val textOriginal = m
        val replacementText = m.replace(p._2, p._3)
        txtOr = txtOr.replace(textOriginal, replacementText)
      }
    }
    txtOr
  }

  val pattern: Regex = "(\\s\"(.*?)\",)".r
  val pattern2: Regex = "([a-z]\\s\"(.*?)\"\\s*[A-Z])".r
  val pattern3: Regex = "(:\\s'\"(.*?)',)".r

  val patternsTR = List(
    (pattern2, "\"", "-u0022"),
    (pattern, "'", "-u0027"),
    (pattern3, "\"", "-u0022")
  )

  val crew = data
    .map(row => row("crew"))
    .map(funcion(_, patternsTR))
    .map(text => text.replace("'", "\""))
    .map(text => text.replace("-u0027", "'"))
    .map(text => text.replace("-u0022", "\\\""))
    .map(text => Try(Json.parse(text)))

  val crewDepartments = crew.map(x => x.get).flatMap(_ \\ "department")
  val crewDepartmentsDistinct = crew.map(x => x.get).flatMap(_ \\ "department").distinct
  val crewDepartmentsGroupBy = crewDepartments.groupBy {
    case department => department
  }.map {
    case department => (department._1, department._2.size)
  }.toList.sortBy(_._2).reverse

  println("Todos los departamentos")
  crewDepartmentsDistinct.foreach(println(_))

  val jobDepartments = crew.map(x => x.get).flatMap(_ \\ "job")
  val jobDepartmentsDistinct = crew.map(x => x.get).flatMap(_ \\ "job").distinct
  val jobDepartmentsGroupBy = crewDepartments.groupBy {
    case job => job
  }.map {
    case job => (job._1, job._2.size)
  }.toList.sortBy(_._2).reverse

  println("Todos los trabajos"+jobDepartmentsDistinct)

  val topFiveDepartments = crewDepartmentsGroupBy.take(5)

  val topFiveName = topFiveDepartments.map(_._1.toString())
  val topFiveCount = topFiveDepartments.map(_._2.toDouble)
  BarChart(topFiveCount)
    .title("Compañías productoras")
    .xAxis(topFiveName)
    .yAxis()
    .frame()
    .yLabel("Productions")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/DatosNoNumericos/Top5Deparments.png"))
  val gender = crew.map(x => x.get).flatMap(_ \\ "gender")
  val genderDistinct = crew.map(x => x.get).flatMap(_ \\ "gender").distinct
  val genderGroupBy = gender.groupBy {
    case gender => gender
  }.map {
    case gender => (gender._1.toString(), gender._2.size.toDouble)
  }.toList.sortBy(_._2)
  BarChart(genderGroupBy.map(_._2))
    .title("Genders")
    .xAxis(genderGroupBy.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/DatosNoNumericos/Gender2.png"))
}
