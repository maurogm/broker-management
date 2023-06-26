package com.maurogm.investments.util

object Utils {
  def resolveErrorEither[A, B](x: Either[A, B]): B = {
    x.fold(
      error => throw new RuntimeException(s"Couldn't resolve Either: $error"),
      value => value
    )
  }

}
