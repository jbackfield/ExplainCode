package com.backfield.explain

import scala.io.StdIn
import scala.reflect.macros.whitebox

import scala.language.experimental.macros

object ExplainCode {

  val colors = List(
    Console.GREEN,
    Console.RED,
    Console.BLUE
  )

  var level = 0

  def pause = {
    StdIn.readLine()
  }

  def explain[A](name : String)(f : => A, pause_? : Boolean, ex : A => Unit) : Unit = macro explainWithNameImpl[A]

  def explain[A](f : => A, pause_? : Boolean, ex : A => Unit) : Unit = macro explainWithoutNameImpl[A]

  def buildTopLine(name : String) : StringBuilder = {
    new StringBuilder().
      append("----[").
      append(name).
      append("]----")
  }

  def execute[A](name : String, pause_? : Boolean, input : A, ex : A => Unit) : Unit = {
    val topLine = buildTopLine(name)
    print(s"${colors(level)}${topLine.toString}${Console.RESET}")
    if(pause_?) { pause } else { println() }
    level = level + 1
    ex(input)
    level = level - 1
    println(s"${colors(level)}${topLine.indices.map(_ => "-").mkString}${Console.RESET}")
  }

  def explainWithNameImpl[A : c.WeakTypeTag](c : whitebox.Context)(name : c.Expr[String])(f : c.Expr[A], pause_? : c.Expr[Boolean], ex : c.Expr[A => Unit]) : c.Expr[Unit] = {
    import c.universe._
    reify {
      execute(name.splice, pause_?.splice, f.splice, ex.splice)
    }
  }

  def explainWithoutNameImpl[A : c.WeakTypeTag](c : whitebox.Context)(f : c.Expr[A], pause_? : c.Expr[Boolean], ex : c.Expr[A => Unit]) : c.Expr[Unit] = {
    import c.universe._
    val tree = c.Expr[String](Literal(Constant(f.tree.toString)))
    reify {
      execute(tree.splice, pause_?.splice, f.splice, ex.splice)
    }
  }
}
