package com.zackmarvel.slox;

case class InterpreterException(msg: String) extends Exception

class Interpreter {
    def interpret(exp: Expr): Unit = {
    }

    def eval(exp: Expr): Any = {
        exp match {
            case Equality(left, equal, right) => evalEquality(left, equal, right)
            case Comparison(left, typ, right) => evalComparison(left, typ, right)
            case Addition(left, typ, right) => evalAddition(left, typ, right)
            case Multiplication(left, typ, right) => evalMultiplication(left, typ, right)
            case Unary(typ, expr) => evalUnary(typ, expr)
            case False => false
            case True => true
            case Number(n) => n
            case Str(s) => s
            case NIL => Nil
        }
    }

    def evalEquality(left: Expr, equal: Boolean, right: Expr): Boolean = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        if (leftValue.isInstanceOf[Float] && rightValue.isInstanceOf[Float]) {
            if (equal) {
                equalEqual[Float](leftValue, rightValue)
            } else {
                bangEqual[Float](leftValue, rightValue)
            }
        } else if (leftValue.isInstanceOf[String] && rightValue.isInstanceOf[String]) {
            if (equal) {
                equalEqual[String](leftValue, rightValue)
            } else {
                bangEqual[String](leftValue, rightValue)
            }
        } else if (leftValue.isInstanceOf[Boolean] && rightValue.isInstanceOf[Boolean]) {
            if (equal) {
                equalEqual[Boolean](leftValue, rightValue)
            } else {
                bangEqual[Boolean](leftValue, rightValue)
            }
        } else {
            throw InterpreterException("Can't compare two different types")
        }
    }

    def equalEqual[T](left: Any, right: Any): Boolean = {
        to[T](left) == to[T](right)
    }

    def bangEqual[T](left: Any, right: Any): Boolean = {
        left != right
    }

    def evalComparison(left: Expr, typ: ComparisonType.Value, right: Expr): Boolean = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        if (leftValue.isInstanceOf[Float] && rightValue.isInstanceOf[Float]) {
            typ match {
                case ComparisonType.Greater =>
                    to[Float](leftValue) > to[Float](rightValue)
                case ComparisonType.GreaterEqual =>
                    to[Float](leftValue) >= to[Float](rightValue)
                case ComparisonType.Less =>
                    to[Float](leftValue) < to[Float](rightValue)
                case ComparisonType.LessEqual =>
                    to[Float](leftValue) <= to[Float](rightValue)
            }
        } else {
            throw InterpreterException("Can't compare two different types")
        }
    }

    def evalAddition(left: Expr, typ: AdditionType.Value, right: Expr): Any = {
        val leftValue = eval(left)
        val rightValue = eval(right)
        if (leftValue.isInstanceOf[Float] && rightValue.isInstanceOf[Float]) {
            typ match {
                case AdditionType.Plus => to[Float](leftValue) + to[Float](rightValue)
                case AdditionType.Minus => to[Float](leftValue) - to[Float](rightValue)
            }
        } else if (leftValue.isInstanceOf[String] && rightValue.isInstanceOf[String]) {
            typ match {
                case AdditionType.Plus => to[String](leftValue) + to[String](rightValue)
                case _ => throw InterpreterException("Subtraction not defined for strings")
            }
        } else {
            throw InterpreterException("Can only add numbers or strings")
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
        if (exprValue.isInstanceOf[Float]) {
            typ match {
                case UnaryType.Minus => -to[Float](exprValue)
                case _ => throw InterpreterException("Unary - is only defined for numbers")
            }
        } else if (exprValue.isInstanceOf[Boolean]) {
            typ match {
                case UnaryType.Bang => !toBoolean(exprValue)
                case _ => throw InterpreterException("Unary ! is only defined for booleans")
            }
        } else {
            throw InterpreterException("Unary operations are only defined for numbers or booleans")
        }
    }

    def to[T](value: Any): T = {
        try {
            value.asInstanceOf[T]
        } catch {
            case err: ClassCastException => {
                throw new InterpreterException("Could not interpret value as requested type")
            }
        }
    }

    def toBoolean(value: Any): Boolean = {
        if (value == Nil) {
            false
        } else if (value.isInstanceOf[Boolean]) {
            value.asInstanceOf[Boolean]
        } else {
            return true
        }
    }

}