package dev.argon.util

trait Lens[S, A] {
  def get(s: S): A
  def set(s: S)(a: A): S
  def modify(s: S)(f: A => A): S = set(s)(f(get(s)))
}
