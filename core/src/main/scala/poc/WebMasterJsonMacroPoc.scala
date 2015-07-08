package poc

import ms.webmaster.macroserialization.Json
import poc.TestData.Person

object WebMasterJsonMacroPoc {

  def main(args: Array[String]) {
//    println(Json.pack(List(1,2,3)))
//    println(Json.pack(Person("john", 123)))
     microBench
  }

  def microBench: Unit = {
    val startTime = System.currentTimeMillis()
    for (i <- 1 to 2000) {
      require( Json.unpack[List[Person]](Json.pack(TestData.longPersonList)) == TestData.longPersonList)
    }
    println("took " + ((System.currentTimeMillis() - startTime)) + " ms")
  }
}
