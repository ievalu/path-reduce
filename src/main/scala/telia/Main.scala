package telia

object Main extends App {

  private def isOpposite(el1: String, el2: String): Boolean = el1 match {
    case "NORTH" => el2 == "SOUTH"
    case "SOUTH" => el2 == "NORTH"
    case "EAST"  => el2 == "WEST"
    case "WEST"  => el2 == "EAST"
  }

  private def isValidDirection(el: String): Boolean = Set("NORTH", "SOUTH", "EAST", "WEST").contains(el)

  def pathReduce(arr: List[String]): Either[String, List[String]] = {
    arr match {
      // if supplied list is empty, empty list can be returned right away
      case res @ Nil => Right(res)
      case _         =>
        // if list contains at least one unsupported path, return error
        if (!arr.forall(isValidDirection)) Left("Provided list contains unsupported value for path")
        // else reduce path
        else
          arr.foldLeft(Right(List.empty[String])) {
            // if accumulator is empty, just add the first element to list
            case (Right(Nil), el) =>
              Right(List(el))
            case (Right(acc), el) =>
              // if the last element in accumulator is opposite to the current element, drop the last element
              if (isOpposite(acc.last, el)) Right(acc.dropRight(1))
              // otherwise add current element to the accumulator
              else Right(acc :+ el)
          }
    }
  }
}
