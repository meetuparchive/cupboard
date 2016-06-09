package com.meetup

import cats.data.Xor

package object cupboard {
  type Result[T] = Xor[Throwable, Persisted[T]]
}
