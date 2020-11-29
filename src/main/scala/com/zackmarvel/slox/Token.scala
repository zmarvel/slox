package com.zackmarvel.slox


object TokenType extends Enumeration {
  val LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Minus, Plus,
  Semicolon, Slash, Star, Bang, BangEqual, Equal, EqualEqual, Greater,
  GreaterEqual, Less, LessEqual, Identifier, Str, Number, And, Class,
  Else, False, Fun, For, If, NIL, Or, Return, Super, This, True,
  Val, Var, While, Eof = Value
}

case class Token(typ: TokenType.Value, lexeme: String,
                 literal: Option[Any], line: Int) {

  override def toString: String = {
    s"${typ.toString} $lexeme $literal"
  }
}