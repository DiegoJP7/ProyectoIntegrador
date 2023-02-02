package Avance1
import Avance1.Fechas.yearReleaseList

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import com.github.tototoshi.csv._
import com.cibo.evilplot.plot._

import java.io.File
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._

object Fechas extends App{
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))
  val data = reader.allWithHeaders()

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  val releaseDateList = data
    .map (row => row("release_date"))
    .filter(!_ .equals(""))
    .map(text => LocalDate.parse(text, dateFormatter))

  val yearReleaseList = releaseDateList
  .map(_.getYear)
  printf("Año menor: %d\n", yearReleaseList.min)
  printf("Año mayor: %d\n", yearReleaseList.max)

///////-------------------------------------

//  val yearReleaseList = releaseDateList
//    .map (_.getYear)
//    .map ( _.toDouble)

  val yearReleaseList2 = releaseDateList
    .map("Ejem"->_.getYear.toDouble)

  PieChart(yearReleaseList2)
    .rightLegend()
    .xbounds (1916.0, 2018.0)
    .render ()
    .write(new File("/Users/diegojp/histoYear.png"))

  //



}

