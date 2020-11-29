package com.zackmarvel.slox

import scala.collection.mutable.ArrayBuffer

class Scanner(val source: String, val tokens: ArrayBuffer[Token],
              var pos: Int = 0, var line: Int = 1) {
  def this(source: String) {
    this(source, ArrayBuffer[Token]())
  }

  def scanTokens(): ArrayBuffer[Token] = {
    while (pos < source.length) {
      //            val start = pos
      scanToken() match {
        case Some(tok) => tokens += tok
        case _ => ()
      }
    }

    tokens += Token(TokenType.Eof, "", None, line)
  }

  private val tokenMap = Map(
    '(' -> TokenType.LeftParen,
    ')' -> TokenType.RightParen,
    '{' -> TokenType.LeftBrace,
    '}' -> TokenType.RightBrace,
    ',' -> TokenType.Comma,
    '.' -> TokenType.Dot,
    ';' -> TokenType.Semicolon,
    '-' -> TokenType.Minus,
    '+' -> TokenType.Plus,
    //'/' -> TokenType.Slash,
    '*' -> TokenType.Star,
  )

  private val keywords = Map(
    "and" -> TokenType.And,
    "class" -> TokenType.Class,
    "else" -> TokenType.Else,
    "false" -> TokenType.False,
    "for" -> TokenType.For,
    "fun" -> TokenType.Fun,
    "if" -> TokenType.If,
    "nil" -> TokenType.NIL,
    "or" -> TokenType.Or,
//    "print" -> TokenType.Print,
    "return" -> TokenType.Return,
    "super" -> TokenType.Super,
    "this" -> TokenType.This,
    "true" -> TokenType.True,
    "var" -> TokenType.Var,
    "val" -> TokenType.Val,
    "while" -> TokenType.While,
  )

  def scanToken(): Option[Token] = {
    val maybeTok = source(pos) match {
      case '(' => Some(Token(TokenType.LeftParen, "(", None, line))
      case ')' => Some(Token(TokenType.RightParen, ")", None, line))
      case '{' => Some(Token(TokenType.LeftBrace, "{", None, line))
      case '}' => Some(Token(TokenType.RightBrace, "}", None, line))
      case ',' => Some(Token(TokenType.Comma, ",", None, line))
      case '.' => Some(Token(TokenType.Dot, ".", None, line))
      case ';' => Some(Token(TokenType.Semicolon, ";", None, line))
      case '-' => Some(Token(TokenType.Minus, "-", None, line))
      case '+' => Some(Token(TokenType.Plus, "+", None, line))
      case '*' => Some(Token(TokenType.Star, "*", None, line))
      case '!' =>
        if (matchAhead(pos, '=')) {
          pos += 1
          Some(Token(TokenType.BangEqual, "!=", None, line))
        } else {
          Some(Token(TokenType.Bang, "!", None, line))
        }
      case '=' =>
        if (matchAhead(pos, '=')) {
          pos += 1
          Some(Token(TokenType.EqualEqual, "==", None, line))
        } else {
          Some(Token(TokenType.Equal, "=", None, line))
        }
      case '<' =>
        if (matchAhead(pos, '=')) {
          pos += 1
          Some(Token(TokenType.LessEqual, "<=", None, line))
        } else {
          Some(Token(TokenType.Less, "<", None, line))
        }
      case '>' =>
        if (matchAhead(pos, '=')) {
          pos += 1
          Some(Token(TokenType.GreaterEqual, ">=", None, line))
        } else {
          Some(Token(TokenType.Greater, ">", None, line))
        }
      case '/' =>
        if (matchAhead(pos, '/')) {
          pos += 1
          while (!matchAhead(pos , '\n') && !atEof(pos)) {
            pos += 1
          }
          None
        } else {
          Some(Token(TokenType.Slash, "/", None, line))
        }
      case '\n' =>
        line += 1
        None
      case c if c.isWhitespace =>
        None
      case '"' =>
        pos += 1
        scanString() match {
          case Some(str) => Some(Token(TokenType.Str, str, Some(str), line))
          case None =>
            // Back up pos by 1 to negate the `pos += 1` at the
            // end of the function
            pos -= 1
            None
        }
      case c if c.isDigit =>
        scanNumber() match {
          case Some(num) => Some(Token(TokenType.Number, num.toString, Some(num), line))
          case _ => None
        }
      case c if c.isLetter =>
        scanKeyword() match {
          case Some(keyword) => keywords.get(keyword) match {
            case Some(kwType) => Some(Token(kwType, keyword, None, line))
            case _ => Some(Token(TokenType.Identifier, keyword, None, line))
          }
          case _ => None
        }
      case _ =>
        Lox.error(Some(line), s"Unexpected token: ${source(pos)}")
        None
    }
    pos += 1
    maybeTok
  }

  def matchAhead(pos: Int, sym: Char): Boolean = {
    peek(pos) match {
      case Some(c) => source(pos+1) == sym
      case None => false
    }
  }

  def peek(pos: Int): Option[Char] = {
    if (!atEof(pos+1)) {
      Some(source(pos+1))
    } else {
      None
    }
  }

  def atEof(pos: Int): Boolean = {
    pos >= source.length
  }

  def scanString(): Option[String] = {
    val slice = source.slice(pos, source.length)
    if (slice.length == 0) {
      None
    } else {
      val str = slice.takeWhile(_ != '"')
      pos += str.length
      if (atEof(pos)) {
        None
      } else {
        Some(str)
      }
    }
  }

  def scanNumber(): Option[Float] = {
    // Read number before decimal point
    val start = pos
    val whole = source.slice(start, source.length).
      takeWhile(_.isDigit)
    if (whole.length > 0) {
      val end = start + whole.length
      peek(end-1) match {
        case Some('.') =>
          val frac: String = source.slice(end+1, source.length).
            takeWhile(_.isDigit)
          if (frac.length > 0) {
            val frac_end = start + whole.length + frac.length
            pos += frac_end - 1
            Some(source.substring(start, frac_end).toFloat)
          } else {
            None
          }
        case _ =>
          pos += whole.length - 1
          Some(whole.toFloat)
      }
    } else {
      None
    }
  }

  def scanKeyword(): Option[String] = {
    if (source(pos).isDigit) {
      None
    } else {
      val kw = source.slice(pos, source.length).
        takeWhile(_.isLetterOrDigit)
      pos += kw.length - 1
      Some(kw)
    }
  }
}