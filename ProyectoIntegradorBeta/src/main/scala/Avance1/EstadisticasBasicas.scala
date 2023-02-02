package Avance1

import java.io.File
import com.github.tototoshi.csv._
object EstadisticasBasicas extends App {
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))
  val data = reader.allWithHeaders()

  reader.close()
  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Presupuesto")
  val budget = data.flatMap(x => x.get("budget"))
  val budgetName = data.map(x => (x.get("budget"), x.get("original_title")) match {
    case (b, o) => (b.get.toDouble, o.get)
  })
  val sumBug = budget.map(x => x.toDouble).sum

  println("Presupuesto mas alto:")
  println(budgetName.maxBy(_._1)._2 + " // " + budgetName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(sumBug / budget.size + "\n")
  println("Presupuesto mas bajo:")
  //Sin 0
  val minBud=budgetName.filter(x => x ._1 != 0.0).min
  println( minBud._2+ " // " + minBud._1 + "\n")

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 ID")
  val id = data.flatMap(x => x.get("id"))
  val idName = data.map(x => (x.get("id"), x.get("original_title")) match {
    case (i, o) => (i.get.toInt, o.get)
  })
  val sumID = id.map(x => x.toDouble).sum

  println("ID mas alto:")
  println(idName.maxBy(_._1)._2 + " // " + idName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(sumID / id.size + "\n")
  println("ID mas bajo:")
  println(idName.minBy(_._1)._2 + " // " + idName.minBy(_._1)._1 + "\n")

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Popularidad")
  val pupularity = data.flatMap(x => x.get("popularity"))
  val popularityName = data.map(x => (x.get("popularity"), x.get("original_title")) match {
    case (p, o) => (p.get.toDouble, o.get)
  })
  val sumPop = pupularity.map(x => x.toDouble).sum

  println("La popularidad mas alta es:")
  println(popularityName.maxBy(_._1)._2 + " // " + popularityName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(sumPop / pupularity.size + "\n")
  println("La popularidad mas baja es:")
  val minPop=popularityName.filter(x=>x._1 != 0.0).min
  println( minPop._2 + " // " + minPop._1 + "\n")

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Recaudacion")
  val revenue = data.flatMap(x => x.get("revenue"))
  val revenueName = data.map(x => (x.get("revenue"), x.get("original_title")) match {
    case (r, o) => (r.get.toDouble, o.get)
  })
  val sumRev = revenue.map(x => x.toDouble).sum

  println("La recaudacion mas alta es:")
  println(revenueName.maxBy(_._1)._2 + " // " + revenueName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(sumRev / revenue.size + "\n")
  //(sin contar el 0)
  println("La recaudacion mas baja:")
  val minRev=revenueName.filter(x=>x._1 != 0.0).min
  println( minRev._2+ " // " +  minRev._1+ "\n")

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Calificacion")

  val vote = data.flatMap(x => x.get("vote_average"))
  val voteName = data.map(x => (x.get("vote_average"), x.get("original_title")) match {
    case (v, o) => (v.get, o.get)
  })
  val sumCal = vote.map(x => x.toDouble).sum
  println("La calificacion mas alta es:")
  println(voteName.maxBy(_._1)._2 + " // " + voteName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(sumCal / vote.size + "\n")
  println("La calificacion mas baja es:")
  val minCal=voteName.filter(x=>x._1 !="0.0").min
  println(minCal._2+ " // " + minCal._1+ "\n")

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Duracion")
  val runtime = data.flatMap(x => x.get("runtime"))
  val runtimeName = data.map(x => (x.get("runtime"), x.get("original_title")) match {
    case (r, o) => (r.get, o.get)
  })
  println("La duracion mas alta es:")
  println(runtimeName.maxBy(_._1)._2 + " // " + runtimeName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(runtime.flatten.sum / runtime.size + "\n")
  println("La duracion mas baja es:")
  val minTime=runtimeName.filter(x=>x._1 != "0").min
  println(minTime._1+ " // " +minTime._2 + "\n")

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Conteo de Votos")
  val votecount = data.flatMap(x => x.get("vote_count"))
  val votecountName = data.map(x => (x.get("vote_count"), x.get("original_title")) match {
    case (vc, o) => (vc.get.toDouble, o.get)
  })
  val sumVC = votecount.map(x => x.toDouble).sum

  println("El conteo mas alto es:")
  println(votecountName.maxBy(_._1)._2 + " // " + votecountName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  println(sumVC / votecount.size + "\n")
  println("El conteo mas bajo es:")
  val minCont=votecountName.filter(x=>x._1 != 0.0)(1)
  println(minCont._2 + " // " + minCont._1 + "\n")

}
