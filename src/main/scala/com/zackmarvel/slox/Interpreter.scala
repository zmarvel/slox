package com.zackmarvel.slox

case class InterpreterException(msg: String) extends Exception

private sealed trait Addition extends Any
private case class FloatAddition(val result: Float) extends AnyVal with Addition
private case class StringAddition(val result: String) extends AnyVal with Addition

class Interpreter {
    def eval(exp: Expr): Any = {
        exp match {
            case Equality(left, equal, right, _) => evalEquality(left, equal, right)
            case Comparison(left, typ, right, _) => evalComparison(left, typ, right)
            case Addition(left, typ, right, _) => evalAddition(left, typ, right)
            case Multiplication(left, typ, right, _) => evalMultiplication(left, typ, right)
            case Unary(typ, expr, _) => evalUnary(typ, expr)
            case False(_) => false
            case True(_) => true
            case Number(n, _) => n
            case Str(s, _) => s
            case NIL(_) => Nil
        }
    }

    type ErrorMessage = String
    type Result[T] = Either[T, ErrorMessage]

    def evalEquality(left: Expr, equal: Boolean, right: Expr): Result[Boolean] = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        if (leftValue.isInstanceOf[Float] && rightValue.isInstanceOf[Float]) {
            if (equal) {
                Left(equalEqual[Float](leftValue, rightValue))
            } else {
                Left(bangEqual[Float](leftValue, rightValue))
            }
        } else if (leftValue.isInstanceOf[String] && rightValue.isInstanceOf[String]) {
            if (equal) {
                Left(equalEqual[String](leftValue, rightValue))
            } else {
                Left(bangEqual[String](leftValue, rightValue))
            }
        } else if (leftValue.isInstanceOf[Boolean] && rightValue.isInstanceOf[Boolean]) {
            if (equal) {
                Left(equalEqual[Boolean](leftValue, rightValue))
            } else {
                Left(bangEqual[Boolean](leftValue, rightValue))
            }
        } else {
            Right("Can't compare two different types")
        }
    }

    def equalEqual[T](left: Any, right: Any): Boolean = {
        to[T](left) == to[T](right)
    }

    def bangEqual[T](left: Any, right: Any): Boolean = {
        left != right
    }

    def evalComparison(left: Expr, typ: ComparisonType.Value, right: Expr): Result[Boolean] = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        if (leftValue.isInstanceOf[Float] && rightValue.isInstanceOf[Float]) {
            typ match {
                case ComparisonType.Greater =>
                    Left(to[Float](leftValue) > to[Float](rightValue))
                case ComparisonType.GreaterEqual =>
                    Left(to[Float](leftValue) >= to[Float](rightValue))
                case ComparisonType.Less =>
                    Left(to[Float](leftValue) < to[Float](rightValue))
                case ComparisonType.LessEqual =>
                    Left(to[Float](leftValue) <= to[Float](rightValue))
            }
        } else {
            Right("Can't compare two different types")
        }
    }

    def evalAddition(left: Expr, typ: AdditionType.Value, right: Expr): Result[Addition] = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        (leftValue, rightValue) match {
            case (lv: Float, rv: Float) => typ match {
                case AdditionType.Plus => Left(FloatAddition(to[Float](leftValue) + to[Float](rightValue)))
                case AdditionType.Minus => Left(FloatAddition(to[Float](leftValue) - to[Float](rightValue)))
            }
            case (lv: String, rv: String) => typ match {
                case AdditionType.Plus => Left(StringAddition(to[String](leftValue) + to[String](rightValue)))
                case _ => Right("Subtraction not defined for strings")
            }
            case _ => Right("Can only add numbers or strings")
        }
    }

    def evalMultiplication(left: Expr, typ: MultiplicationType.Value, right: Expr): Any = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        if (leftValue.isInstanceOf[Float] && rightValue.isInstanceOf[Float]) {
            typ match {
                case MultiplicationType.Star => to[Float](leftValue) * to[Float](rightValue)
                case MultiplicationType.Slash => to[Float](leftValue) / to[Float](rightValue)
            }
        } else {
            throw InterpreterException("Can only multiply numbers")
        }
    }

    def evalUnary(typ: UnaryType.Value, expr: Expr): Any = {
        val exprValue = eval(expr)
        exprValue match {
            case _: Float =>
                typ match {
                    case UnaryType.Minus => -to[Float](exprValue)
                    case _ => throw InterpreterException("Unary - is only defined for numbers")
                }
            case _: Boolean =>
                typ match {
                    case UnaryType.Bang => !toBoolean(exprValue)
                    case _ => throw InterpreterException("Unary ! is only defined for booleans")
                }
            case _ =>
                throw InterpreterException("Unary operations are only defined for numbers or booleans")
        }
    }

    def to[T](value: Any): T = {
        try {
            value.asInstanceOf[T]
        } catch {
            case _: ClassCastException => {
                throw InterpreterException("Could not interpret value as requested type")
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