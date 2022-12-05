import scala.annotation.tailrec

case class Move(quantity: Int, from: Int, to: Int)

implicit class ListOps[A](list: List[A]) {
  def pop(amount: Int): (List[A], List[A]) = {
    @tailrec
    def helper(containers: List[A], stack: List[A], counter: Int): (List[A], List[A]) =
      if (counter <= 0) (stack, containers)
      else {
        containers match {
          case Nil          => (stack, containers)
          case head :: tail => helper(tail, head :: stack, counter - 1)
        }
      }

    helper(containers = list, stack = List.empty, counter = amount)
  }
}

val containersMap = Map(
  1 -> List("W", "L", "S"),
  2 -> List("Q", "N", "T", "J"),
  3 -> List("J", "F", "H", "C", "S"),
  4 -> List("B", "G", "N", "W", "M", "R", "T"),
  5 -> List("B", "Q", "H", "D", "S", "L", "R", "T"),
  6 -> List("L", "R", "H", "F", "V", "B", "J", "M"),
  7 -> List("M", "J", "N", "R", "W", "D"),
  8 -> List("J", "D", "N", "H", "F", "T", "Z", "B"),
  9 -> List("T", "F", "B", "N", "Q", "L", "H")
)

val containersMap2 = Map(
  1 -> List("N", "Z"),
  2 -> List("D", "C", "M"),
  3 -> List("P")
)

/*
val moves = List(
  Move(5, 4, 5),
  Move(2, 5, 8),
  Move(2, 9, 1),
  Move(2, 9, 1),
  Move(1, 5, 3),
  Move(10, 5, 8)
)

 */

val moves2 = List(
  Move(1, 2, 1),
  Move(3, 1, 3),
  Move(2, 2, 1),
  Move(1, 1, 2)
)

def rearrange(moves: List[Move]): Map[Int, List[String]] =
  moves
    .foldLeft(containersMap2) { (seed, current) =>
      println(s"move: $current")
      println(s"containers before update: $seed")
      val (poppedContainers, remaining) = seed(current.from).pop(current.quantity)
      val updatedContainers             = poppedContainers ::: seed(current.to)

      println(s"popped (amount: ${current.quantity}): $poppedContainers")
      println(s"remaining (from ${current.from}): $remaining")
      println(s"updated to ${current.to} : $updatedContainers")
      println("----")

      seed
        .updated(current.from, remaining)
        .updated(current.to, updatedContainers)
    }

rearrange(moves2)
