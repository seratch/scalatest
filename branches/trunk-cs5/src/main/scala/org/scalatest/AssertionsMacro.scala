package org.scalatest

import reflect.makro.Context
import collection.mutable.ListBuffer
import collection.immutable.TreeMap
import scala.tools.nsc.util.{OffsetPosition, RangePosition}

class AssertionsMacro[C <: Context](val context: C) {
  import context.mirror._

  def apply(condition: Expr[Boolean]): Expr[Unit] = {
    val text: Expr[String] = context.literal(getErrorMessage(condition.tree))
    context.reify {
      if (!condition.eval) {
        throw Assertions.newAssertionFailedException(Some(text.eval), None, 4)
      }
    }
  }

  def getErrorMessage(tree: Tree): String = {
    tree match {
      case apply: Apply =>
        if (apply.args.size == 1) {
          apply.fun match {
            case select: Select =>
              select.name.decoded match {
                case "==" =>
                  select.qualifier + " did not equal to " + apply.args(0)
                case "!=" =>
                  select.qualifier + " was equal to " + apply.args(0)
                case ">" =>
                  select.qualifier + " did not more than " + apply.args(0)
                case ">=" =>
                  select.qualifier + " did not more than or equal " + apply.args(0)
                case "<" =>
                  select.qualifier + " did not less than " + apply.args(0)
                case "<=" =>
                  select.qualifier + " did not less than or equal " + apply.args(0)
                case _ =>
                  getText(tree) + " fails."
              }
            case _ =>
              getText(tree) + " fails."
          }
        }
        else
          getText(tree) + " fails."
      case _ =>
        getText(tree) + " fails."
    }
  }

  def getText(expr: Tree): String = expr.pos match {
    case p: RangePosition => context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start, p.end).trim
    case p: scala.tools.nsc.util.Position => p.lineContent.trim
  }
}

object AssertionsMacro {
  def apply(context: Context)(condition: context.Expr[Boolean]): context.Expr[Unit] = {
    new AssertionsMacro[context.type](context).apply(condition)
  }
}
