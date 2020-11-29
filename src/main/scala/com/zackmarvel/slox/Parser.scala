package com.zackmarvel.slox

case class ParseError(tok: Option[Token], msg: String)


class Parser(val tokens: Array[Token], var pos: Int = 0) {
  def parse(): Either[Seq[Stmt], ParseError] = {
    program()
  }

  // TODO this could return a list of ParseError too
  def program(stmts: Vector[Stmt] = Vector[Stmt]()): Either[Seq[Stmt], ParseError] = {
    peek() match {
      case tok if isEof(tok) => Left(stmts)
      case None => Left(stmts)
      case _ => statement() match {
        case Left(stmt) => program(stmts :+ stmt)
        case Right(err) => Right(err)
      }
    }
  }

  def isEof(tok: Option[Token]): Boolean = {
    tok match {
      case Some(Token(TokenType.Eof, _, _, _)) => true
      case _ => false
    }
  }

  def statement(): Either[Stmt, ParseError] = {
    peek() match {
      case Some(Token(TokenType.Val, _, _, _)) =>
        read()
        val result = valDeclaration()
        result
      case Some(Token(TokenType.Var, _, _, _)) =>
        read()
        val result = varDeclaration()
        result
      case _ => exprStmt()
    }
  }

  def exprStmt(): Either[Stmt, ParseError] = {
    expression() match {
      case Left(expr) =>
        read() match {
          case Some(Token(TokenType.Semicolon, _, _, _)) => Left(ExprStmt(expr))
          case maybeTok => Right(ParseError(maybeTok, "Expected semicolon following statement"))
        }
      case Right(err) => Right(err)
    }

  }

  def valDeclaration(): Either[Stmt, ParseError] = declaration(ValDeclaration)

  def varDeclaration(): Either[Stmt, ParseError] = declaration(VarDeclaration)

  def declaration[T <: Stmt](constructor: (Token, Expr) => T): Either[Stmt, ParseError] = {
    read() match {
      case Some(identifierTok @ Token(TokenType.Identifier, name, _, _)) =>
        read() match {
          case Some(Token(TokenType.Equal, _, _, _)) =>
            expression() match {
              case Left(expr) =>
                read() match {
                  case Some(Token(TokenType.Semicolon, _, _, _)) => Left(constructor(identifierTok, expr))
                  case maybeTok => Right(ParseError(maybeTok, "Expected semicolon following statement"))
                }
              case Right(err) => Right(err)
            }
          case tok => Right(ParseError(tok, "Expected `='"))
        }
      case tok => Right(ParseError(tok, "Expected identifier"))
    }
  }

  def expression(): Either[Expr, ParseError] = {
    assignment()
  }

  def assignment(): Either[Expr, ParseError] = {
    val lhs = equality()
    peek() match {
      case Some(Token(TokenType.Equal, _, _, _)) =>
        read()
        val rhs = assignment()
        lhs match {
          case Left(Identifier(tok)) =>
            rhs match {
              case Left(rexpr) => Left(Assignment(rexpr, tok))
              case Right(err) => Right(err)
            }
          case Right(err) => Right(err)
            // TODO handle non-identifier types later
          case Left(lexpr @ _) => Right(ParseError(lexpr.token, "Invalid lhs for assignment"))
        }
      case _ => lhs
    }
  }

  def equality(): Either[Expr, ParseError] = {
    var left = comparison()
    while (isEqualityOperator(peek())) {
      val equalityOperator = read()
      left match {
        case Left(lexpr) =>
          comparison() match {
            case Left(rexpr) =>
              equalityOperator match {
                case Some(tok) if tok.typ == TokenType.EqualEqual =>
                  left = Left(Equality(lexpr, equal = true, rexpr, Some(tok)))
                case Some(tok) if tok.typ == TokenType.BangEqual =>
                  left = Left(Equality(lexpr, equal = false, rexpr, Some(tok)))
                case _ => Right(ParseError(peek(), "Expect equality op"))
              }
            case err => err
          }
        case err => err
      }
    }
    left
  }

  def isEqualityOperator(tok: Option[Token]): Boolean = {
    tok match {
      case Some(Token(TokenType.EqualEqual, _, _, _)) => true
      case Some(Token(TokenType.BangEqual, _, _, _)) => true
      case _ => false
    }
  }

  private val comparisonOperatorType = Map(
    TokenType.Greater -> ComparisonType.Greater,
    TokenType.GreaterEqual -> ComparisonType.GreaterEqual,
    TokenType.Less -> ComparisonType.Less,
    TokenType.LessEqual -> ComparisonType.LessEqual,
  )

  def comparison(): Either[Expr, ParseError] = {
    var left = addition()
    while (isComparisonOperator(peek())) {
      val comparisonOperator = read()
      left match {
        case Left(lexpr) =>
          addition() match {
            case Left(rexpr) =>
              comparisonOperator match {
              case Some(tok) =>
                left = Left(Comparison(lexpr, comparisonOperatorType(tok.typ), rexpr, Some(tok)))
              case _ => Right(ParseError(peek(), "Expect comparison op"))
            }
            case err => err
          }
        case err => err
      }
    }
    left
  }

  def isComparisonOperator(tok: Option[Token]): Boolean = {
    tok match {
      case Some(Token(typ, _, _, _)) =>
        comparisonOperatorType.get(typ) match {
          case Some(_) => true
          case _ => false
        }
      case _ => false
    }
  }

  private val additionOperatorType = Map(
    TokenType.Minus -> AdditionType.Minus,
    TokenType.Plus -> AdditionType.Plus,
  )

  def addition(): Either[Expr, ParseError] = {
    var left = multiplication()
    while (isAdditionOperator(peek())) {
      val additionOperator = read()
      left match {
        case Left(lexpr) =>
          multiplication() match {
            case Left(rexpr) =>
              additionOperator match {
                case Some(tok) =>
                  left = Left(Addition(lexpr, additionOperatorType(tok.typ), rexpr, Some(tok)))
                case _ => Right(ParseError(peek(), "Expect addition op"))
              }
            case err => err
          }
        case err => err
      }
    }
    left
  }

  def isAdditionOperator(tok: Option[Token]): Boolean = {
    tok match {
      case Some(Token(typ, _, _, _)) =>
        additionOperatorType.get(typ) match {
          case Some(_) => true
          case _ => false
        }
      case _ => false
    }
  }

  private val multOperatorType = Map(
    TokenType.Star -> MultiplicationType.Star,
    TokenType.Slash -> MultiplicationType.Slash,
  )

  def multiplication(): Either[Expr, ParseError] = {
    var left = unary()
    while (isMultOperator(peek())) {
      val multOperator = read()
      unary() match {
        case Left(rexpr) =>
          multOperator match {
            case Some(tok) =>
              left match {
                case Left(expr) =>
                  left = Left(Multiplication(expr, multOperatorType(tok.typ), rexpr, Some(tok)))
                case err => err
              }
            case _ => Right(ParseError(peek(), "Expect multiplication op"))
          }
        case err => err
      }
    }
    left
  }

  def isMultOperator(tok: Option[Token]): Boolean = {
    tok match {
      case Some(Token(typ, _, _, _)) =>
        multOperatorType.get(typ) match {
          case Some(_) => true
          case _ => false
        }
      case _ => false
    }
  }

  private val unaryOperatorType = Map(
    TokenType.Bang -> UnaryType.Bang,
    TokenType.Minus -> UnaryType.Minus,
  )

  def unary(): Either[Expr, ParseError] = {
    if (isUnaryOperator(peek())) {
      val unaryOperator = read()
      unary() match {
        case Left(rand) => unaryOperator match {
          case Some(tok) => Left(Unary(unaryOperatorType(tok.typ), rand, Some(tok)))
          case _ => Right(ParseError(peek(), "Expect unary op"))
        }
        case err => err
      }
    } else {
      primary()
    }
  }

  def isUnaryOperator(maybeTok: Option[Token]): Boolean = {
    maybeTok match {
      case Some(tok) =>
        unaryOperatorType.get(tok.typ) match {
          case Some(_) => true
          case _ => false
        }
      case _ => false
    }
  }

  def primary(): Either[Expr, ParseError] = {
    read() match {
      case Some(tok) =>
        tok.typ match {
          case TokenType.Number => Left(Number(tok.literal.get.asInstanceOf[Float], Some(tok)))
          case TokenType.Str => Left(Str(tok.lexeme, Some(tok)))
          case TokenType.False => Left(False(Some(tok)))
          case TokenType.True => Left(True(Some(tok)))
          case TokenType.NIL => Left(NIL(Some(tok)))
          // TODO pass this token to the grouping
          case TokenType.Identifier => Left(identifier(tok))
          case TokenType.LeftParen => grouping()
          case _ => Right(ParseError(Some(tok), "Expected expression"))
        }
      case tok => Right(ParseError(tok, "Expected expression"))
    }
  }

  // Assume the left paren was read by the caller
  def grouping(): Either[Expr, ParseError] = {
    expression() match {
      case Left(inner) =>
        read() match {
          case Some(tok) if tok.typ == TokenType.RightParen =>
            Left(Grouping(inner, Some(tok)))
          case _ => Right(ParseError(peek(), "Expect closing paren"))
        }
      case err => err
    }
  }

  def identifier(tok: Token): Expr = {
    Identifier(Some(tok))
  }

  def read(): Option[Token] = {
    if (pos < tokens.length) {
      val tok = tokens(pos)
      pos += 1
      Some(tok)
    } else {
      None
    }
  }

  def peek(): Option[Token] = {
    if (pos < tokens.length) {
      Some(tokens(pos))
    } else {
      None
    }
  }

}