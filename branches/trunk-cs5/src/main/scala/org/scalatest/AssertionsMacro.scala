package org.scalatest

import reflect.makro.Context
import scala.tools.nsc.util.RangePosition
import collection.mutable.ListBuffer
import collection.immutable.TreeMap

class AssertionsMacro[C <: Context](val context: C) {
  import context.mirror._

  def apply(recording: Expr[Boolean]): Expr[Boolean] = {
    Expr(Block(declareRuntime :: recordExpressions(recording.tree), completeRecording))
  }

  private[this] def declareRuntime: Tree = {
    val runtimeClass = staticClass(classOf[AssertionRecorderRuntime].getName)
    ValDef(
      Modifiers(),
      newTermName("$org_scalatest_recorderRuntime"),
      TypeTree(runtimeClass.asType),
      Apply(
        Select(
          New(Ident(runtimeClass)),
          newTermName("<init>")),
        List(
          Select(
            context.prefix.tree,
            newTermName("listener")))))
  }

  private[this] def recordExpressions(recording: Tree): List[Tree] = {
    val exprs = splitExpressions(recording)
    exprs.flatMap { expr =>
      val text = getText(expr)
      val ast = showRaw(expr)
      try {
        List(resetValues, recordExpression(text, ast, expr))
      } catch {
        case e => throw new RuntimeException(
          "ScalaTest: Error rewriting expression.\nText: " + text + "\nAST : " + ast, e)
      }
    }
  }

  private[this] def completeRecording: Tree =
    Apply(
      Select(
        Ident(newTermName("$org_scalatest_recorderRuntime")),
        newTermName("completeRecording")),
      List())

  private[this] def resetValues: Tree =
    Apply(
      Select(
        Ident(newTermName("$org_scalatest_recorderRuntime")),
        newTermName("resetValues")),
      List())

  private[this] def recordExpression(text: String, ast: String, expr: Tree) = {
    val buggedExpr = recordAllValues(expr)
    log(expr, "Expression  : " + text.trim())
    log(expr, "Original AST: " + ast)
    log(expr, "Bugged AST  : " + showRaw(buggedExpr))
    log(expr, "")

    Apply(
      Select(
        Ident(newTermName("$org_scalatest_recorderRuntime")),
        newTermName("recordExpression")),
      List(
        context.literal(text).tree,
        context.literal(ast).tree,
        buggedExpr))
  }

  private[this] def splitExpressions(recording: Tree): List[Tree] = recording match {
    case Block(xs, y) => xs ::: List(y)
    case _ => List(recording)
  }

  private[this] def recordAllValues(expr: Tree): Tree = expr match {
    case New(_) => expr // only record after ctor call
    case Literal(_) => expr // don't record
    // don't record value of implicit "this" added by compiler; couldn't find a better way to detect implicit "this" than via point
    case Select(x@This(_), y) if getPosition(expr).point == getPosition(x).point => expr
    case _ => recordValue(recordSubValues(expr), expr)
  }

  private[this] def recordSubValues(expr: Tree) : Tree = expr match {
    case Apply(x, ys) => Apply(recordAllValues(x), ys.map(recordAllValues(_)))
    case TypeApply(x, ys) => recordValue(TypeApply(recordSubValues(x), ys), expr)
    case Select(x, y) => Select(recordAllValues(x), y)
    case _ => expr
  }

  private[this] def recordValue(expr: Tree, origExpr: Tree): Tree =
    if (origExpr.tpe.typeSymbol.isType)
      Apply(
        Select(
          Ident(newTermName("$org_scalatest_recorderRuntime")),
          newTermName("recordValue")),
        List(expr, Literal(Constant(getAnchor(origExpr)))))
    else expr

  private[this] def getText(expr: Tree): String = expr.pos match {
    case p: RangePosition => context.echo(expr.pos, "RangePosition found!"); p.lineContent.slice(p.start, p.end)
    case p: scala.tools.nsc.util.Position => p.lineContent
  }

  private[this] def getAnchor(expr: Tree): Int = expr match {
    case Apply(x, ys) => getAnchor(x) + 0
    case TypeApply(x, ys) => getAnchor(x) + 0
    case _ => {
      val pos = getPosition(expr)
      pos.point - pos.source.lineToOffset(pos.line - 1)
    }
  }

  private[this] def getPosition(expr: Tree) = expr.pos.asInstanceOf[scala.tools.nsc.util.Position]

  private[this] def log(expr: Tree, msg: String) {
    context.info(expr.pos, msg, false)
  }

}

object AssertionsMacro {
  def apply(context: Context)(recording: context.Expr[Boolean]): context.Expr[Boolean] = {
    new AssertionsMacro[context.type](context).apply(recording)
  }
}

class AssertionRecorderRuntime(listener: AssertionRecorderListener[Boolean]) {
  var recordedValues: List[AssertionRecordedValue] = _
  var recordedExprs: List[AssertionRecordedExpression[Boolean]] = List.empty

  def resetValues() {
    recordedValues = List.empty
  }

  def recordValue[U](value: U, anchor: Int): U = {
    val recordedValue = AssertionRecordedValue(value, anchor)
    listener.valueRecorded(recordedValue)
    recordedValues = recordedValue :: recordedValues
    value
  }

  def recordExpression(text: String, ast: String, value: Boolean) {
    val recordedExpr = AssertionRecordedExpression(text, ast, value, recordedValues)
    listener.expressionRecorded(recordedExpr)
    recordedExprs = recordedExpr :: recordedExprs
  }

  def completeRecording(): Boolean = {
    val lastRecorded = recordedExprs.head
    val recording = AssertionRecording(lastRecorded.value, recordedExprs)
    listener.recordingCompleted(recording)
    recording.value
  }
}

class AssertionRecorderListener[T](failEarly: Boolean = true, showTypes: Boolean = false,
                                   printAsts: Boolean = false, printExprs: Boolean = false) {
  def valueRecorded(recordedValue: AssertionRecordedValue) {}

  def expressionRecorded(recordedExpr: AssertionRecordedExpression[Boolean]) {
    lazy val rendering = new AssertionExpressionRenderer(showTypes).render(recordedExpr)
    if (printAsts) println(recordedExpr.ast + "\n")
    if (printExprs) println(rendering)
    if (!recordedExpr.value && failEarly) {
      //throw new AssertionError("\n\n" + rendering)
      throw Assertions.newAssertionFailedException(Some("\n\n" + rendering), None, 4)
    }
  }

  def recordingCompleted(recording: AssertionRecording[Boolean]) {
    if (!failEarly) {
      val failedExprs = recording.recordedExprs.filter(!_.value)
      if (!failedExprs.isEmpty) {
        val renderer = new AssertionExpressionRenderer(showTypes)
        val renderings = failedExprs.reverse.map(renderer.render(_))
        //throw new AssertionError("\n\n" + renderings.mkString("\n\n"))
        throw Assertions.newAssertionFailedException(Some("\n\n" + renderings.mkString("\n\n")), None, 4)
      }
    }
  }
}

case class AssertionRecordedValue(value: Any, anchor: Int)

case class AssertionRecordedExpression[T](text: String, ast: String, value: T, recordedValues: List[AssertionRecordedValue])

case class AssertionRecording[T](value: T, recordedExprs: List[AssertionRecordedExpression[T]])

/*abstract class AssertionRecorder {
  val listener: AssertionRecorderListener[Boolean]
  import language.experimental.macros
  def apply(recording: Boolean): Boolean = macro AssertionsMacro.apply
}*/

/*class AssertionRecorder {
  val listener: AssertionRecorderListener[Boolean] = new AssertionRecorderListener[Boolean]()
  import language.experimental.macros
  def apply(recording: Boolean): Boolean = macro AssertionsMacro.apply
} */

class AssertionExpressionRenderer(showTypes: Boolean) {
  def render(recordedExpr: AssertionRecordedExpression[_]): String = {
    val offset = recordedExpr.text.prefixLength(_.isWhitespace)
    val intro = new StringBuilder().append(recordedExpr.text.trim())
    val lines = ListBuffer(new StringBuilder)

    val rightToLeft = filterAndSortByAnchor(recordedExpr.recordedValues)
    for (recordedValue <- rightToLeft) placeValue(lines, recordedValue.value, recordedValue.anchor - offset)

    lines.prepend(intro)
    lines.append(new StringBuilder)
    lines.mkString("\n")
  }

  private[this] def filterAndSortByAnchor(recordedValues: List[AssertionRecordedValue]): Traversable[AssertionRecordedValue] = {
    var map = TreeMap[Int, AssertionRecordedValue]()(Ordering.by(-_))
    // values stemming from compiler generated code often have the same anchor as regular values
    // and get recorded before them; let's filter them out
    for (value <- recordedValues) if (!map.contains(value.anchor)) map += (value.anchor -> value)
    map.values
  }

  private[this] def placeValue(lines: ListBuffer[StringBuilder], value: Any, col: Int) {
    val str = renderValue(value)

    placeString(lines(0), "|", col)

    for (line <- lines.drop(1)) {
      if (fits(line, str, col)) {
        placeString(line, str, col)
        return
      }
      placeString(line, "|", col)
    }

    val newLine = new StringBuilder()
    placeString(newLine, str, col)
    lines.append(newLine)
  }

  private[this] def renderValue(value: Any): String = {
    val str = if (value == null) "null" else value.toString
    if (showTypes) str + " (" + scala.reflect.mirror.typeOfInstance(value).typeSymbol.fullName + ")"
    else str
  }

  private[this] def placeString(line: StringBuilder, str: String, anchor: Int) {
    val diff = anchor - line.length
    for (i <- 1 to diff) line.append(' ')
    line.replace(anchor, anchor + str.length(), str)
  }

  private[this] def fits(line: StringBuilder, str: String, anchor: Int): Boolean = {
    line.slice(anchor, anchor + str.length() + 1).forall(_.isWhitespace)
  }
}