package Avance1

import java.io.File

object SvgPlot extends App{
  import com.cibo.evilplot._
  import com.cibo.evilplot.plot._
  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._

  import com.cibo.evilplot.plot._

  val data = Seq("one" -> 1.5, "two" -> 3.5, "three" -> 2.0)
  PieChart(data).rightLegend().render()
    .write(new File("/Users/diegojp/histoYearreg.png"))
}
