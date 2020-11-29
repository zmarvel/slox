package com.zackmarvel.slox

// expression     → assignment ;
// assignment     → IDENTIFIER "=" assignment
//                | equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "false" | "true" | "nil"
//                | "(" expression ")" ;

trait Expr {
  val token: Option[Token]
}


object OperatorType extends Enumeration {
  val Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
  Plus, Minus, Star, Slash = Value
}

object ComparisonType extends Enumeration {
  val Greater, GreaterEqual, Less, LessEqual = Value
}

object AdditionType extends Enumeration {
  val Plus, Minus = Value
}

object MultiplicationType extends Enumeration {
  val Star, Slash = Value
}

object UnaryType extends Enumeration {
  val Bang, Minus = Value
}


case class Equality(left: Expr, equal: Boolean, right: Expr, token: Option[Token])
  extends Expr
case class Comparison(left: Expr, typ: ComparisonType.Value, right: Expr, token: Option[Token])
  extends Expr
case class Addition(left: Expr, typ: AdditionType.Value, right: Expr, token: Option[Token])
  extends Expr
case class Multiplication(left: Expr, typ: MultiplicationType.Value, right: Expr, token: Option[Token])
  extends Expr
case class Unary(typ: UnaryType.Value, rand: Expr, token: Option[Token])
  extends Expr
case class Number(value: Float, token: Option[Token])
  extends Expr
case class Str(value: String, token: Option[Token])
  extends Expr
case class False(token: Option[Token])
  extends Expr
case class True(token: Option[Token])
  extends Expr
case class NIL(token: Option[Token])
  extends Expr
// Name provided by token
case class Identifier(token: Option[Token])
  extends Expr
case class Assignment(expr: Expr, token: Option[Token])
  extends Expr
case class Grouping(expr: Expr, token: Option[Token])
  extends Expr
