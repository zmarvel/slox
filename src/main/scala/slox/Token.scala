package com.zackmarvel.slox;


object TokenType extends Enumeration {
    val LeftParen, RightParen, LeftBrace, RightBrace, Comma, Dot, Minus, Plus,
        Semicolon, Slash, Star, Bang, BangEqual, Equal, EqualEqual, Greater,
        GreaterEqual, Less, LessEqual, Identifier, Str, Number, And, Class,
        Else, False, Fun, For, If, NIL, Or, Print, Return, Super, This, True,
        Var, While, Eof = Value
}

case class Token(val typ: TokenType.Value, val lexeme: String,
            val literal: Option[Any], val line: Int) {
    
    override def toString(): String = {
        return s"${typ.toString} $lexeme $literal"
    }
}