package Avance1
import java.io.File
import com.github.tototoshi.csv._
object AnalisisDatosTxT  extends App{
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))
  val data = reader.allWithHeaders()

  reader.close()
  println("---------------------------------------------------------------------")
  println("Estadisticas\n")
  //Tablas para analizar
  //genres
  //original_language
  //status
  //director
  println("                                                 Genero")
  val genreCount = {
    val genero = data.flatMap(x => x.get("genres"))
    genero.groupBy { case genero => genero }
      .map { case genero => genero }
      .map { case genero => (genero._1, genero._2.size) }
      .toList.sortBy(x => x._2)
  }
  //Saber si esta vacio

  val genresCount = {
    val genero = data.flatMap(x => x.get("genres")).flatMap(x => x.split(" ").toList)
    genero.groupBy {
      case genero => genero
    }.map {
      case genero => (genero._1, genero._2.size)
    }
  }
  println("Tipos de genero que existen y su cantidad \n" + genresCount)

  val genreCountEmp = {
    val genero = data.flatMap(x => x.get("genres"))
    genero.groupBy { case genero => genero.isBlank }
      .map { case genero => genero }
      .map { case genero => (genero._1, genero._2.size) }
      .toList.sortBy(x => x._2)
  }
  println("Las filas vacias en genero son: " + genreCountEmp.map(x => x._2).head)
  println("Las filas llenas en genero son: " + genreCountEmp.map(x => x._2)(1))

  println("---------------------------------------------------------------------")
  println("                                                 Lenguaje")
  val languageCount = {
    val language = data.flatMap(x => x.get("original_language"))
    language.groupBy { case language => language }
      .map { case language => language }
      .map { case language => (language._1, language._2.size) }
      .toList.sortBy(x => x._2)
  }
  println("Lenguajes: " + languageCount + "\n")
  println("Mayor Lenguaje: " + languageCount.maxBy(x => x._2))
  println("Menor Lenguaje: " + languageCount.minBy(x => x._2))

  println("---------------------------------------------------------------------")
  println("                                                 Status")
  val ststusCount = {
    val status = data.flatMap(x => x.get("status"))
    status.groupBy { case status => status }
      .map { case status => status }
      .map { case status => (status._1, status._2.size) }
      .toList.sortBy(x => x._2)
  }
  //Para saber si esta vacio
  val statusEmp = {
    val status = data.flatMap(x => x.get("status"))
    status.groupBy { case status => status.isBlank }
      .map { case status => status }
      .map { case status => (status._1, status._2.size) }
      .toList.sortBy(x => x._2)
  }
  println("Status Existentes: " + ststusCount)

  println("---------------------------------------------------------------------")
  println("                                                 Director")
  val directorCount = {
    val director = data.flatMap(x => x.get("director"))
    director.groupBy { case director => director }
      .map { case director => director }
      .map { case director => (director._1, director._2.size) }
      .toList.sortBy(x => x._2)
  }
  //println("Director y numero de peliculas "+directorCount)
  val directorCountEmp = {
    val director = data.flatMap(x => x.get("director"))
    director.groupBy { case director => director.isBlank }
      .map { case director => director }
      .map { case director => (director._1, director._2.size) }
      .toList.sortBy(x => x._2)
      .map(x => x._2)
  }
  println("Peliculas sin director: " + directorCountEmp.head)
  println("Director con mas peliculas: " + directorCount.filter(x => x._1 != "").maxBy(x => x._2))
  println("Director con menos peliculas: " + directorCount.filter(x => x._1 != "").minBy(x => x._2))
}
