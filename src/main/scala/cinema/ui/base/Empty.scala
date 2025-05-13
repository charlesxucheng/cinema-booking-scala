package cinema.ui.base

trait Empty[A] {
  def empty: A
}

object Empty {
  inline def apply[A](using ev: Empty[A]): A = ev.empty
}
