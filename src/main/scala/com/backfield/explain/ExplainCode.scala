package com.backfield.explain

import java.util.function.{Consumer, Supplier}

import scala.io.StdIn
import scala.reflect.macros.whitebox

import scala.language.experimental.macros
import scala.runtime.BoxedUnit

case class ExplainCode(var level : Int = 0)

object ExplainCode {

  var entryPoints : List[Example] = List()

  implicit val globalExplainCode = new ExplainCode(0)

  val colors = List(
    Console.GREEN,
    Console.RED,
    Console.BLUE
  )

  def pause = {
    StdIn.readLine()
  }

  def explain[A](name : String, f : => A, pause_? : Boolean, ex : A => Unit, ec : ExplainCode) : Unit = macro explainWithNameImpl[A]

  def explain[A](f : => A, pause_? : Boolean, ex : A => Unit, ec : ExplainCode) : Unit = macro explainWithoutNameImpl[A]

  def buildTopLine(name : String) : StringBuilder = {
    new StringBuilder().
      append("----[").
      append(name).
      append("]----")
  }

  def execute[A](name : String, pause_? : Boolean, input : A, ex : A => Unit, ec : ExplainCode) : Unit = {
    val topLine = buildTopLine(name)
    print(s"${colors(ec.level)}${topLine.toString}${Console.RESET}")
    if(pause_?) { pause } else { println() }
    ec.level = ec.level + 1
    ex(input)
    ec.level = ec.level - 1
    println(s"${colors(ec.level)}${topLine.indices.map(_ => "-").mkString}${Console.RESET}")
  }

  def explainWithNameImpl[A : c.WeakTypeTag](c : whitebox.Context)(
    name : c.Expr[String],
    f : c.Expr[A],
    pause_? : c.Expr[Boolean],
    ex : c.Expr[A => Unit],
    ec : c.Expr[ExplainCode]
  ) : c.Expr[Unit] = {
    import c.universe._
    reify {
      execute(name.splice, pause_?.splice, f.splice, ex.splice, ec.splice)
    }
  }

  def explainWithoutNameImpl[A : c.WeakTypeTag](c : whitebox.Context)(
    f : c.Expr[A],
    pause_? : c.Expr[Boolean],
    ex : c.Expr[A => Unit],
    ec : c.Expr[ExplainCode]
  ) : c.Expr[Unit] = {
    import c.universe._
    val tree = c.Expr[String](Literal(Constant(f.tree.toString)))
    reify {
      execute(tree.splice, pause_?.splice, f.splice, ex.splice, ec.splice)
    }
  }

  def iterate(commands : List[Int] = List()) : List[Int] = {
    print("Choice: ")
    val line = StdIn.readLine()
    try {
      val input = line.trim.toInt
      if(entryPoints.length < input) {
        println(s"Invalid choice $input")
        iterate(commands)
      } else if(input == -1) {
        println("Goodbye")
        commands
      } else if(input == 0) {
        var i = 1
        entryPoints.foreach { ep =>
          val done = if (ep.done) {
            s"${Console.GREEN}√${Console.RESET}"
          } else {
            s"${Console.RED}-${Console.RESET}"
          }
          println(s"$i. ${ep.name}...[$done]")
          i = i + 1
        }
        iterate(commands)
      } else if(input < -1) {
        println(s"Invalid choice $input")
        iterate(commands)
      } else {
        val entryPoint = entryPoints(input - 1)
        println(s"${Console.GREEN}${entryPoint.name}${Console.RESET}")
        entryPoint.execute()
        iterate(input :: commands)
      }
    } catch {
      case _ : Throwable => {
        println(s"Invalid choice")
        iterate(commands)
      }
    }

  }

  def addExample(example : Example) = {
    entryPoints = entryPoints ::: List(example)
  }

}
