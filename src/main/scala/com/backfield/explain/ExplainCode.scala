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
    Console.BLUE,
    Console.CYAN,
    Console.WHITE
  )

  def pause : Unit = {
    StdIn.readLine()
  }

  def explain[A](name : String, f : => A, ex : A => Unit, ec : ExplainCode) : Unit = macro explainWithNameImpl[A]

  def explain[A](f : => A, ex : A => Unit, ec : ExplainCode) : Unit = macro explainWithoutNameImpl[A]

  def getSeparator(line : String) : String = {
    val maxLine = line.split("\n").maxBy(_.length).length
    (0 until maxLine).map(_ => "-").mkString
  }

  def buildTopLine(name : String) : StringBuilder = {
    val isMultiLine = name.contains("\n")
    val separator = getSeparator(name)
    val sb = new StringBuilder()
    if(isMultiLine) {
      sb.append(separator)
      sb.append("\n")
    } else {
      sb.append("----")
      sb.append("[")
    }
    sb.append(name)
    if(isMultiLine) {
      sb.append("\n")
      sb.append(separator)
    } else {
      sb.append("]")
      sb.append("----")
    }
  }

  def execute[A](name : String, input : A, ex : A => Unit, ec : ExplainCode) : Unit = {
    val topLine = buildTopLine(name).toString
    print(s"${colors(ec.level)}$topLine${Console.RESET}")
    pause
    ec.level = ec.level + 1
    ex(input)
    ec.level = ec.level - 1
    val separator = getSeparator(topLine)
    println(s"${colors(ec.level)}$separator${Console.RESET}")
  }

  def explainWithNameImpl[A : c.WeakTypeTag](c : whitebox.Context)(
    name : c.Expr[String],
    f : c.Expr[A],
    ex : c.Expr[A => Unit],
    ec : c.Expr[ExplainCode]
  ) : c.Expr[Unit] = {
    import c.universe._
    reify {
      execute(name.splice, f.splice, ex.splice, ec.splice)
    }
  }

  def explainWithoutNameImpl[A : c.WeakTypeTag](c : whitebox.Context)(
    f : c.Expr[A],
    ex : c.Expr[A => Unit],
    ec : c.Expr[ExplainCode]
  ) : c.Expr[Unit] = {
    import c.universe._
    val tree = c.Expr[String](Literal(Constant(f.tree.toString)))
    reify {
      execute(tree.splice, f.splice, ex.splice, ec.splice)
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
      case ec : Throwable => {
        println(s"Invalid choice")
        ec.printStackTrace()
        iterate(commands)
      }
    }

  }

  def addExample(example : Example) = {
    entryPoints = entryPoints ::: List(example)
  }

}
