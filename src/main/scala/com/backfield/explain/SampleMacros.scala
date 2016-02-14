package com.backfield.explain

import scala.reflect.macros.whitebox

import scala.language.experimental.macros

object SampleMacros {

  def test(ourTest : Boolean) : Unit = macro test_impl

  def createList(c : whitebox.Context)(tree : c.universe.Tree) : c.Expr[Unit] = {
    import c.universe._
    tree match {
      case q"$a && $b" => reify {
        createList(c)(a).splice
        createList(c)(b).splice
      }
      case q"$a == $b" => {
        val left = c.Expr[String](Literal(Constant(a.toString)))
        val right = c.Expr[String](Literal(Constant(b.toString)))
        val exec = c.Expr[Boolean](tree)
        reify {
          if(exec.splice) {
            println(s"${left.splice} == ${right.splice}")
          } else {
            println(s"${left.splice} != ${right.splice}")
          }
        }
      }
    }
  }

  def test_impl(c : whitebox.Context)(ourTest : c.Expr[Boolean]) : c.Expr[Unit] = {
    import c.universe._
    val output = createList(c)(ourTest.tree)
    reify {
      output.splice
    }
  }

}
