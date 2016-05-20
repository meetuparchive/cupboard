package com.meetup.cupboard

package object models {
  case class Foo(s: String, i: Int)
  case class Bar(i: Int, f: Foo)
}

