package poc

object TestData {
  case class Person(name: String, age: Int)

  val longPersonList = (1 to 1000).map(idx => Person("John", idx)).toList
}
