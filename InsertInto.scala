import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import requests.Response

import java.io.File
import play.api.libs.json._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import play.api.libs.json.{JsArray, JsValue, Json}
import scalikejdbc._

import scala.util.{Failure, Success, Try}

object InsertInto extends App{
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))
  val data: List[Map[String,String]]= {
    reader.allWithHeaders()
  }
  reader.close()
  Class.forName("com.mysql.cj.jdbc.Driver")
  ConnectionPool.singleton("jdbc:mysql://localhost:3306/MoviesTS14", "root", "messer10")
  implicit val session: DBSession = AutoSession

  case class Movie(id: String,
                   originalLanguage: String,
                   originalTitle: String,
                   budget: Long,
                   popularity: Double,
                   runtime: Double,
                   revenue: Long)

  val movieData = data
    .map(row => Movie(
      row("id"),
      row("original_language"),
      row("original_title"),
      row("budget").toLong,
      row("popularity").toDouble,
      row("runtime") match {
        case valueOfRT if valueOfRT.trim.isEmpty => 0.0
        case valueOfRT => valueOfRT.toDouble
      },
      row("revenue").toLong))

  /*  val SQL_INSERT_PATTERN =
      """INSERT INTO MOVIE (`RAW_ID`, `ORIGINAL_TITLE`, `BUDGET`, `POPULARITY`,`RUNTIME`, `REVENUE`)
        |VALUES
        |('%s', '%s', %d, %f, %f, %d, '%s');
        |""".stripMargin*/

  val SQL_INSERT_PATTERN =
    """INSERT INTO MOVIE (`RAW_ID`, `ORIGINAL_TITLE`)
      |VALUES
      |('%s', '%s');
      |""".stripMargin

  /*  val scriptData = movieData
      .map(movie => SQL_INSERT_PATTERN.formatLocal(java.util.Locale.US,
        movie.id,
        escapeMysql(movie.originalTitle),
        movie.budget,
        movie.popularity,
        movie.runtime,
        movie.revenue,
        escapeMysql(movie.originalLanguage)
      ))*/
  val scriptData = movieData
    .map(movie => SQL_INSERT_PATTERN.formatLocal(java.util.Locale.US,
      movie.id,
      escapeMysql(movie.originalTitle)
    ))
  val scriptFile = new File("/Users/diegojp/Desktop/NuevoMovie.sql")
  if(scriptFile.exists()) scriptFile.delete()

  scriptData.foreach(insert =>
    Files.write(Paths.get("/Users/diegojp/Desktop/NuevoMovie.sql"), insert.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
  )

  def escapeMysql(text: String) : String = text
    .replaceAll("\\\\", "\\\\\\\\")
    .replaceAll("\b", "\\\\b")
    .replaceAll("\n", "\\\\n")
    .replaceAll("\r", "\\\\r")
    .replaceAll("\t", "\\\\t")
    .replaceAll("\\x1A", "\\\\Z")
    .replaceAll("\\x00", "\\\\0")
    .replaceAll("'", "\\\\'")
    .replaceAll("\"", "\\\\\"")

  def actorsNames(dataRaw: String): Option[String] = {
    val response: Response = requests
      .post("http://api.meaningcloud.com/topics-2.0",
        data = Map("key" -> "4c3a20ef3efdfbf831ed7c77088f5e58",
          "lang" -> "en",
          "txt" -> dataRaw,
          "tt" -> "e"),
        headers = Map("content-type" -> "application/x-www-form-urlencoded"))
    Thread.sleep(500)
    if(response.statusCode == 200) {
      Option(response.text)
    } else
      Option.empty
  }

  /*val cast = data
    .map(row => row("cast"))
    .filter(_.nonEmpty)
    .map(StringContext.processEscapes)
    .take(15) //Use un número limitado para hacer sus pruebas, pero, al final debe analizar todos los datos.
    .map(actorsNames)
    .map(json => Try(Json.parse(json.get)))
    .filter(_.isSuccess)
    .map(_.get)
    .flatMap(json => json("entity_list").as[JsArray].value)
    .map(_("form"))
    .map(data => data.as[String])
    .toSet*/

  def transform(jsValue: JsValue): List[String] =
    jsValue("entity_list").as[JsArray].value
      .map(_("form"))
      .map(_.as[String])
      .toList

  val castId = data
    .map(row => (row("id"), row("original_title"),row("cast"),row("revenue")))
    .filter(_._2.nonEmpty)
    .map(tuple4 => (tuple4._1,tuple4._2, StringContext.processEscapes(tuple4._3),tuple4._4))
    .take(10)
    .map{t4=>(t4._1,t4._2,actorsNames(t4._3),t4._4)}
    .map{t4=>(t4._1,t4._2,Try(Json.parse(t4._3.get)),t4._4)}
    .filter(_._3.isSuccess)
    .map(t4=>(t4._1,t4._2,t4._3.get,t4._4))
    .map(t4=>(t4._1,t4._2,transform(t4._3),t4._4))
    .flatMap(t4=>t4._3.map(name=>(t4._1,t4._2,name,t4._4)))
    .map(_.productIterator.toList)
    .distinct
  //Tablas
  val castData = data
    .map((row) => row("cast"))
    .filter(_.nonEmpty)
    .take(10)
    .map(StringContext.processEscapes)
    .map(actorsNames)
    .map(json => Try(Json.parse(json.get)))
    .filter(_.isSuccess)
    .map(_.get)
    .flatMap(json => json("entity_list").as[JsArray].value)
    .map(_("form"))
    .map(data => data.as[String])
    .distinct
    .toSet

  val cast = castData.map(x =>
    sql"""
         INSERT INTO `cast`(nombreCast)
         VALUES
         (${x})
         """.stripMargin
      .update
      .apply())



  val f = new File("/Users/diegojp/Desktop/NuevoMovie.csv")
  val writer = CSVWriter.open(f)
  writer.writeAll(castId)
  writer.close()
}

