package com.zackmarvel.slox

case class InterpreterException(msg: String) extends Exception

class Interpreter {
  private val environment = new Environment()

  def interpret(program: Program): Unit = {
    for (statement <- program.body) {
      execute(statement)
    }
  }

  def execute(statement: Stmt): Unit = {
    statement match {
      case ExprStmt(expr) => eval(expr)
      case ValDeclaration(name, expr) => environment.bindImmutable(name.lexeme, eval(expr))
      case VarDeclaration(name, expr) => environment.bindMutable(name.lexeme, eval(expr))
    }
  }

  def eval(exp: Expr): Result[Any] = {
    exp match {
      case Equality(left, equal, right, _) => evalEquality(left, equal, right)
      case Comparison(left, typ, right, _) => evalComparison(left, typ, right)
      case Addition(left, typ, right, _) => evalAddition(left, typ, right)
      case Multiplication(left, typ, right, _) => evalMultiplication(left, typ, right)
      case Unary(typ, expr, _) => evalUnary(typ, expr)
      case False(_) => Left(false)
      case True(_) => Left(true)
      case Number(n, _) => Left(n)
      case Str(s, _) => Left(s)
      case NIL(_) => Left(())
    }
  }

  private type ErrorMessage = String
  private type Result[T] = Either[T, ErrorMessage]

  def evalEquality(left: Expr, equal: Boolean, right: Expr): Result[Boolean] = {
    val leftValue = eval(left)
    val rightValue = eval(right)

    def comparisonFunc(x: Any, y: Any) = if (equal) { equalEqual(x, y) } else { bangEqual(x, y) }
      (leftValue, rightValue) match {
        case (Left(lv: Float), Left(rv: Float)) => Left(comparisonFunc(lv, rv))
        case  (Left(lv: String), Left(rv: String)) => Left(comparisonFunc(lv, rv))
        case  (Left(lv: Boolean), Left(rv: Boolean)) => Left(comparisonFunc(lv, rv))
        case (Right(err), _) => Right(err)
        case (_, Right(err)) => Right(err)
        case _ => Right("Can't compare two different types")
      }
  }

  def equalEqual[T](left: T, right: T): Boolean = {
    left == right
  }

  def bangEqual[T](left: T, right: T): Boolean = {
    left != right
  }

  def evalComparison(left: Expr, typ: ComparisonType.Value, right: Expr): Result[Boolean] = {
    val leftValue = eval(left)
    val rightValue = eval(right)
    def comparisonFunc(x: Float, y: Float): Boolean = typ match {
      case ComparisonType.Greater => x > y
      case ComparisonType.GreaterEqual => x >= y
      case ComparisonType.Less => x < y
      case ComparisonType.LessEqual => x <= y
    }

    (leftValue, rightValue) match {
      case (Left(lv: Float), Left(rv: Float)) => Left(comparisonFunc(lv, rv))
      case (Right(err), _) => Right(err)
      case (_, Right(err)) => Right(err)
      case _ => Right("Can't compare two different types")
    }
  }

  def evalAddition(left: Expr, typ: AdditionType.Value, right: Expr): Result[Any] = {
    val leftValue = eval(left)
    val rightValue = eval(right)
    (leftValue, rightValue) match {
      case (Left(lv: Float), Left(rv: Float)) => typ match {
        case AdditionType.Plus => Left(lv + rv)
        case AdditionType.Minus => Left(lv - rv)
      }
      case (Left(lv: String), Left(rv: String)) => typ match {
        case AdditionType.Plus => Left(lv + rv)
        case _ => Right("Subtraction not defined for strings")
      }
      case (Right(err), _) => Right(err)
      case (_, Right(err)) => Right(err)
      case _ => Right("Can only add numbers or strings")
    }
  }

  def evalMultiplication(left: Expr, typ: MultiplicationType.Value, right: Expr): Result[Any] = {
    val leftValue = eval(left)
    val rightValue = eval(right)
    (leftValue, rightValue) match {
      case (Left(lv: Float), Left(rv: Float)) =>
        typ match {
          case MultiplicationType.Star => Left(lv * rv)
          case MultiplicationType.Slash => Left(lv / rv)
        }
      case (Right(err), _) => Right(err)
      case (_, Right(err)) => Right(err)
      case _ => Right("Can only multiply numbers")
    }
  }

  def evalUnary(typ: UnaryType.Value, expr: Expr): Result[Any] = {
    val exprValue = eval(expr)
    typ match {
      case UnaryType.Minus =>
        exprValue match {
          case Left(rand: Float) => Left(-rand)
          case Right(err) => Right(err)
          case _ => Right("Unary - is only defined for numbers")
        }
      case UnaryType.Bang =>
        exprValue match {
          case Left(rand: Boolean) => Left(!toBoolean(rand))
          case Right(err) => Right(err)
          case _ => Right("Unary ! is only defined for booleans")
        }
    }
  }

  def toBoolean(value: Any): Boolean = {
    value match {
      case Nil => false
      case value: Boolean => value.asInstanceOf[Boolean]
      case _ => true
    }
  }

}