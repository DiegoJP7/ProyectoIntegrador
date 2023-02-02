package Avance1

import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.DefaultTheme.{DefaultElements, DefaultTheme}
import com.github.tototoshi.csv.CSVReader
import play.api.libs.json.{JsArray, JsValue, Json}

import java.io.File
object Crew extends App{
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))

  val data : List[Map[String,String]]={
    reader.allWithHeaders()
  }
  reader.close()

  val productionCompanies = data
  .flatMap (row => row. get ("production _companies"))
  .map (row => Json.parse (row))
    .flatMap (jsonData => jsonData \\ "name")
   .map (jsValue => jsValue.as[String])
  .groupBy(identity)
    .map { case (keyword, lista) => (keyword, lista.size) }
    .toList
  .sortBy(_._2)
  .reverse

  val pCompaniesValues = productionCompanies.take(10).map(_._2).map(_.toDouble)
  val pCompaniesLables = productionCompanies.take(10).map(_._1)

  implicit val theme = DefaultTheme.copy(
    elements = DefaultElements.copy(categoricalXAxisLabelOrientation = 45)
  )

  BarChart(pCompaniesValues)
    .title("Compañías productoras")
    .xAxis(pCompaniesLables)
    .yAxis()
    .frame()
    .yLabel("Productions")
    .bottomLegend()
    .render()
    .write(new File ("/Users/diegojp/crew.png"))


  //Wiki poner imagenes cada analisis
}
