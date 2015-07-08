package poc

import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization
import poc.TestData.Person

object Json4s {

  def main(args: Array[String]) {
    implicit val format = DefaultFormats

    val startTime = System.currentTimeMillis()
    for( i <- 1 to 2000 ) {
      Serialization.read[List[Person]](Serialization.write(TestData.longPersonList))
    }
    println("took " + (System.currentTimeMillis() - startTime) + " ms")
  }

}
