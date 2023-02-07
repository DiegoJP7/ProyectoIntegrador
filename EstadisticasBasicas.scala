package Avance1
import com.cibo.evilplot.plot.BarChart
import com.cibo.evilplot.plot.aesthetics.DefaultTheme.{DefaultElements, DefaultTheme}
import java.io.File
import com.github.tototoshi.csv._

object EstadisticasBasicas extends App {
  val reader = CSVReader.open(new File("/Users/diegojp/Downloads/movie_dataset 2.csv"))
  val data = reader.allWithHeaders()
  reader.close()
  //
  implicit val theme = DefaultTheme.copy(
    elements = DefaultElements.copy(categoricalXAxisLabelOrientation = 45)
  )

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Presupuesto")
  val budget = data.flatMap(x => x.get("budget"))
  val budgetName = data.map(x => (x.get("budget"), x.get("original_title")) match {
    case (b, o) => (b.get.toDouble, o.get)
  })
  val budgetTopMax= data.map(x => (x.get("budget"), x.get("original_title")) match {
    case (b, o) => (b.get.toDouble, o.get)
  }).sortBy(_._1).reverse.take(5)
  val budgetTopMin= data.map(x => (x.get("budget"), x.get("original_title")) match {
    case (b, o) => (b.get.toDouble, o.get)
  }).filter(b=>b._1 !=  0.0).sortBy(_._1).take(10)

  val sumBug0 = budget.map(x => x.toDouble).sum
  val sumBug = budget.map(x=>x.toDouble).filter(x=>x != 0.0).sum
  //Cual es el Presupuesto mas alto?
  println("Presupuesto mas alto:")
  val mayor=budgetName.maxBy(_._1)._2
  println(mayor + " // " + budgetName.maxBy(_._1)._1 + "\n")
  //Promedio contando 0
  println("Promedio con 0")
  val promBug0=sumBug0 / budget.size
  println(promBug0 + "\n")
  println("Promedio sin 0")
  val promBug=sumBug / budget.map(x => x.toDouble).count(x => x != 0.0)
  println(promBug+"\n")
  println("Presupuesto mas bajo:")
  //Cual es el presupuesto mas bajo con 0?
  val minW0=budgetName.min
  println(minW0._2+ " // " + minW0._1 + "\n")
  //Cual es el Presupuesto mas bajo sin 0?
  println("Presupuesto mas bajo sin 0:")
  val minBud=budgetName.filter(x => x ._1 != 0.0).min
  println( minBud._2+ " // " + minBud._1 + "\n")

  //Diagrama
  val budgetDiagram=List(
    ("Presupuesto mas alto :"+"\n"+budgetName.maxBy(_._1)._2,budgetName.maxBy(_._1)._1 ),
    ("Presupuesto mas bajo :"+"\n"+budgetName.minBy(_._1)._2,minBud._1),
    ("Presupuesto mas bajo contando 0:"+"\n" +minW0._2,minW0._1),
    ("Promedio", promBug) ,
    ("Promedio con 0", promBug0)
  )
  BarChart(budgetDiagram.map(_._2))
    .title("Presupuesto")
    .xAxis(budgetDiagram.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Budget.png")
    )

  //Diagrama Top

  BarChart(budgetTopMax.map(_._1.toDouble))
    .title("Las 5 Peliculas Con Mayor Presupuesto")
    .xAxis(budgetTopMax.map(_._2))
    .yAxis()
    .ybounds(100000000)
    .frame()
    .yLabel("Valores")
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/BudgetTopMax.png")
    )

  BarChart(budgetTopMin.map(_._1.toDouble))
    .title("Las 10 Peliculas Con Menor Presupuesto")
    .xAxis(budgetTopMin.map(_._2))
    .yAxis()
    .ybounds(0.5)
    .frame()
    .yLabel("Valores")
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/BudgetTopMin.png")
    )
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
  val popularityTopMax= data.map(x => (x.get("popularity"), x.get("original_title")) match {
    case (p, o) => (p.get.toDouble, o.get)
  }).sortBy(_._1).reverse.take(5)
  val popularityTopMin= data.map(x => (x.get("popularity"), x.get("original_title")) match {
    case (p, o) => (p.get.toDouble, o.get)
  }).sortBy(_._1).take(5)
  val sumPop = pupularity.map(x => x.toDouble).sum

  println("La popularidad mas alta es:")
  println(popularityName.maxBy(_._1)._2 + " // " + popularityName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  val promPop=sumPop / pupularity.size
  println( promPop+ "\n")
  println("La Popularidad mas baja sin 0 es:")
  val minPopW0=popularityName.minBy(_._1)
  println(minPopW0._2 + " // " + minPopW0._1+ "\n")
  println("La popularidad mas baja es:")
  val minPop=popularityName.filter(x=>x._1 != 0.0).min
  println( minPop._2 + " // " + minPop._1 + "\n")

  val popDiagram=List(
    ("Popularidad mas alta :"+"\n"+popularityName.maxBy(_._1)._2,popularityName.maxBy(_._1)._1.toDouble ),
    ("Popularidad mas baja :"+"\n"+minPop._2,minPop._1.toDouble),
    ("Popularidad mas baja sin 0: "+"\n"+minPopW0._2,minPopW0._1),
    ("Promedio", promPop)
  )
  BarChart(popDiagram.map(_._2))
    .title("Popularidad")
    .xAxis(popDiagram.map(_._1))
    .yAxis()
    .ybounds(5)
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Popularity.png")
    )
  //Diagrama Top
  BarChart(popularityTopMax.map(_._1))
    .title("Top 5 popularidades mas altas")
    .xAxis(popularityTopMax.map(_._2))
    .yAxis()
    .frame()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/PopularityTopMax.png")
    )

  BarChart(popularityTopMin.map(_._1))
    .title("Top 5 popularidades mas bajas")
    .xAxis(popularityTopMin.map(_._2))
    .yAxis()
    .frame()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/PopularityTopMin.png")
    )

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Recaudacion")
  val revenue = data.flatMap(x => x.get("revenue"))
  val revenueName = data.map(x => (x.get("revenue"), x.get("original_title")) match {
    case (r, o) => (r.get.toDouble, o.get)
  })
  val revenueMax=data.map(x => (x.get("revenue"), x.get("original_title")) match {
    case (r, o) => (r.get.toDouble, o.get)
  }).sortBy(_._1).reverse.take(5)
  val revenueMin=data.map(x => (x.get("revenue"), x.get("original_title")) match {
    case (r, o) => (r.get.toDouble, o.get)
  }).filter(r=>r._1 != 0.0).sortBy(_._1).take(5)
  val sumRev = revenue.map(x => x.toDouble).sum

  println("La recaudacion mas alta es:")
  println(revenueName.maxBy(_._1)._2 + " // " + revenueName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  val promRev=sumRev / revenue.size
  println(promRev + "\n")
  //(sin contar el 0)
  println("La recaudacion mas baja:")
  val minRev=revenueName.filter(x=>x._1 != 0.0).min
  println( minRev._2+ " // " +  minRev._1+ "\n")

  println("La recaudacion mas baja contando 0:")
  val minRevW0=revenueName.minBy(_._1)
  println(minRevW0._2+" // "+minRevW0._1)
  val revDiagram=List(
    ("La recaudacion mas alta es:"+"\n"+revenueName.maxBy(_._1)._2,revenueName.maxBy(_._1)._1.toDouble ),
    ("La recaudacion mas baja:"+"\n"+minRev._2,minRev._1.toDouble),
    ("La recaudacion mas baja contando 0:"+"\n"+minRevW0._2,minRevW0._1),
    ("Promedio", promRev)
  )
  BarChart(revDiagram.map(_._2))
    .title("Revenue")
    .xAxis(revDiagram.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Revenue.png")
    )
  //Diagramas Top
  BarChart(revenueMax.map(_._1))
    .title("Las 5 Peliculas con mas recaudacion")
    .xAxis(revenueMax.map(_._2))
    .yAxis()
    .ybounds(1000000000)
    .frame()
    .yLabel("Valores")
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/RevenueTopMax.png")
    )

  BarChart(revenueMin.map(_._1))
    .title("Las 5 Peliculas con menos recaudacion")
    .xAxis(revenueMin.map(_._2))
    .yAxis()
    .ybounds(1)
    .frame()
    .yLabel("Valores")
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/RevenueTopMin.png")
    )

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Calificacion")

  val vote = data.flatMap(x => x.get("vote_average"))
  val voteName = data.map(x => (x.get("vote_average"), x.get("original_title")) match {
    case (v, o) => (v.get.toDouble, o.get)
  })

  val voteMax= data.map(x => (x.get("vote_average"), x.get("original_title")) match {
    case (v, o) => (v.get.toDouble, o.get)
  }).sortBy(_._1).reverse.take(5)

  val voteMin= data.map(x => (x.get("vote_average"), x.get("original_title")) match {
    case (v, o) => (v.get.toDouble, o.get)
  }).filter(v=>v._1 != 0.0).sortBy(_._1).take(5)

  val sumCal = vote.map(x => x.toDouble).sum
  println("La calificacion mas alta es:")
  println(voteName.maxBy(_._1)._2 + " // " + voteName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  val proVote=sumCal / vote.size
  println(proVote + "\n")
  println("La calificacion mas baja es:")
  val minCal=voteName.filter(x=>x._1 != 0.0).min
  println(minCal._2+ " // " + minCal._1+ "\n")
  println("La calificacion mas baja contando 0 es:")
  val minCalW0=voteName.minBy(_._1)
  println(minCalW0._2+" // "+minCalW0._1)

  val voteDiagram=List(
    ("La calificacion mas alta es:"+"\n"+voteName.maxBy(_._1)._2,voteName.maxBy(_._1)._1.toDouble ),
    ("La calificacion mas baja es:"+"\n"+minCal._2,minCal._1.toDouble),
    ("La calificacion mas baja contando 0 es:"+"\n"+minCalW0._2,minCalW0._1),
    ("Promedio", proVote)
  )
  BarChart(voteDiagram.map(_._2))
    .title("Vote")
    .xAxis(voteDiagram.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Vote.png")
    )
  //Diagramas Top
  BarChart(voteMax.map(_._1))
    .title("Las 5 Peliculas con mejor calificacion")
    .xAxis(voteMax.map(_._2))
    .yAxis()
    .ybounds(9,10)
    .frame()
    .yLabel("Valores")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/VoteTopMax.png")
    )

  BarChart(voteMin.map(_._1))
    .title("Las 5 Peliculas con peor calificacion")
    .xAxis(voteMin.map(_._2))
    .yAxis()
    .ybounds(0.3)
    .frame()
    .yLabel("Valores")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/VoteTopMin.png")
    )

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Duracion")
  val runtime = data.flatMap(x => x.get("runtime")).filter(_.contains(".")).map(x=>x.toDouble)
  val runtimeMax= data.map(x=>(x("original_title"),x("runtime"))).filter(_._2.contains(".")).sortBy(_._2.toDouble).reverse.take(5)
  val runtimeMin= data.map(x => (x("original_title"), x("runtime"))).filter(_._2.contains(".")).filter(_._2.toDouble != 0.0).sortBy(_._2.toDouble).take(5)

  println("La duracion mas alta es:")
  println(runtime.max + "\n")
  println("La duracion mas baja es:")
  val minTime=runtime.filter(x=>x != 0.0).min
  println(minTime +"\n")
  println("La duracion mas baja con 0 es:")
  val minTimeW0=runtime.min
  println(minTimeW0+"\n")

  val timeDiagram=List(
    ("La duracion mas alta es:"+"\n",runtime.max ),
    ("La duracion mas baja es:"+"\n",minTime),
    ("La duracion mas baja con 0 es:", minTimeW0)
  )
  BarChart(timeDiagram.map(_._2))
    .title("Runtime")
    .xAxis(timeDiagram.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Runtime.png")
    )

  //Diagramss Top
  BarChart(runtimeMax.map(_._2.toDouble))
    .title("Las 5 peliculas con mas duracion")
    .xAxis(runtimeMax.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valores")
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/RuntimeMax.png")
    )

  BarChart(runtimeMin.map(_._2.toDouble))
    .title("Las 5 peliculas con menos duracion")
    .xAxis(runtimeMin.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valores")
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/RuntimeMin.png")
    )

  println("--------------------------------------------------------------------------------------------------")
  println("                                                                 Conteo de Votos")
  val votecount = data.flatMap(x => x.get("vote_count"))
  val votecountName = data.map(x => (x.get("vote_count"), x.get("original_title")) match {
    case (vc, o) => (vc.get.toDouble, o.get)
  })
  val votecountMax=data.map(x => (x.get("vote_count"), x.get("original_title")) match {
    case (vc, o) => (vc.get.toDouble, o.get)
  }).sortBy(_._1).reverse.take(5)
  val votecountMin=data.map(x => (x.get("vote_count"), x.get("original_title")) match {
    case (vc, o) => (vc.get.toDouble, o.get)
  }).filter(v=>v._1 != 0.0).filter(v=>v._1 != 1.0).sortBy(_._1).take(10)

  val sumVC = votecount.map(x => x.toDouble).sum

  println("El conteo mas alto es:")
  println(votecountName.maxBy(_._1)._2 + " // " + votecountName.maxBy(_._1)._1 + "\n")
  println("Promedio")
  val promVC =sumVC / votecount.size
  println(promVC + "\n")
  println("El conteo mas bajo es:")
  val minCont=votecountName.filter(x=>x._1 != 0.0)(1)
  println(minCont._2 + " // " + minCont._1 + "\n")

  val VCDiagram=List(
    ("valor mas alto :"+"\n"+votecountName.maxBy(_._1)._2,votecountName.maxBy(_._1)._1.toDouble ),
    ("valor mas bajo :"+"\n"+minCont._2,minCont._1.toDouble),
    ("Promedio", promVC)
  )
  BarChart(VCDiagram.map(_._2))
    .title("Vote Count")
    .xAxis(VCDiagram.map(_._1))
    .yAxis()
    .frame()
    .yLabel("Valor")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Vote-Count.png"))

  BarChart(votecountMax.map(_._1))
    .title("Top 5 Peliculas con mas votos")
    .xAxis(votecountMax.map(_._2))
    .yAxis()
    .frame()
    .yLabel("Valores")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Vote-CountMax.png"))

  BarChart(votecountMin.map(_._1))
    .title("Top 5 Peliculas con mas votos")
    .xAxis(votecountMin.map(_._2))
    .yAxis()
    .frame()
    .yLabel("Valores")
    .bottomLegend()
    .render()
    .write(new File("/Users/diegojp/Desktop/diagms/Vote-CountMin.png"))
}
