package com.zackmarvel.slox;

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "false" | "true" | "nil"
//                | "(" expression ")" ;

trait Expr


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

object PrimaryType extends Enumeration {
    val Number, Str, Bool, NIL = Value
}



case class Equality(left: Expr, equal: Boolean, right: Expr)
    extends Expr
case class Comparison(left: Expr, typ: ComparisonType.Value, right: Expr)
    extends Expr
case class Addition(left: Expr, typ: AdditionType.Value, right: Expr)
    extends Expr
case class Multiplication(left: Expr, typ: MultiplicationType.Value, right: Expr)
    extends Expr
case class Unary(typ: UnaryType.Value, rand: Expr)
    extends Expr
case class Primary(typ: PrimaryType.Value, value: Any)
    extends Expr
case class Number(value: Float)
    extends Expr
case class Str(value: String)
    extends Expr
case object False
    extends Expr
case object True
    extends Expr
case object NIL
    extends Expr
case class Grouping(expr: Expr)
    extends Expr
