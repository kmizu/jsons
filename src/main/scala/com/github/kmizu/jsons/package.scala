package com.github.kmizu

package object jsons {
  sealed abstract class Union[+A, +B]
  case class ULeft[A](value: A) extends Union[A, Nothing]
  case class URight[A](value: A) extends Union[Nothing, A]
}
