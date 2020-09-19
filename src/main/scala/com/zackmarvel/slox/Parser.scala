package com.zackmarvel.slox

case class ParserException(tok: Option[Token], msg: String) extends Exception

class Parser(val tokens: Array[Token], var pos: Int = 0) {
    def parse(): Either[Expr, ParserException] = {
        try {
            Left(expression())
        } catch {
            case err: ParserException => Right(err)
        }
    }

    def program(stmts: List[Stmt] = List[Stmt]()): List[Stmt] = {
        if (isEof(peek())) {
            stmts
        } else {
            program(statement() :: stmts)
        }
    }

    def isEof(tok: Option[Token]): Boolean = {
        tok match {
            case Some(Token(TokenType.Eof, _, _, _)) => true
            case _ => false
        }
    }

    def statement(): Stmt = {
        peek() match {
            case Some(Token(TokenType.Print, _, _, _)) =>
                read()
                printStmt()
            case _ => exprStmt()
        }
    }

    def exprStmt(): Stmt = {
        val expr = expression()
        read() match {
            case Some(Token(TokenType.Semicolon, _, _, _)) => ExprStmt(expr)
            case maybeTok => throw ParserException(maybeTok, "Expected semicolon following statement")
        }
    }

    def printStmt(): Stmt = {
        // Assume the Print token has already been read
        val expr = expression()
        read() match {
            case Some(Token(TokenType.Semicolon, _, _, _)) => Print(expr)
            case maybeTok => throw ParserException(maybeTok, "Expected semicolon following statement")
        }
    }

    def expression(): Expr = {
        equality()
    }

    def equality(): Expr = {
        var left = comparison()
        while (isEqualityOperator(peek())) {
            val equalityOperator = read()
            val right = comparison()
            equalityOperator match {
                case Some(tok) if tok.typ == TokenType.EqualEqual =>
                    left = Equality(left, equal = true, right, Some(tok))
                case Some(tok) if tok.typ == TokenType.BangEqual =>
                    left = Equality(left, equal = false, right, Some(tok))
                case _ => throw ParserException(peek(), "Expect equality op")
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

    def comparison(): Expr = {
        var left = addition()
        while (isComparisonOperator(peek())) {
            val comparisonOperator = read()
            val right = addition()
            comparisonOperator match {
                case Some(tok) =>
                    left = Comparison(left, comparisonOperatorType(tok.typ), right, Some(tok))
                case _ => throw ParserException(peek(), "Expect comparison op")
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

    def addition(): Expr = {
        var left = multiplication()
        while (isAdditionOperator(peek())) {
            val additionOperator = read()
            val right = multiplication()
            additionOperator match {
                case Some(tok) =>
                    left = Addition(left, additionOperatorType(tok.typ), right, Some(tok))
                case _ => throw ParserException(peek(), "Expect addition op")
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

    def multiplication(): Expr = {
        var left = unary()
        while (isMultOperator(peek())) {
            val multOperator = read()
            val right = unary()
            multOperator match {
                case Some(tok) =>
                    left = Multiplication(left, multOperatorType(tok.typ), right, Some(tok))
                case _ => throw ParserException(peek(), "Expect multiplication op")
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

    def unary(): Expr = {
        if (isUnaryOperator(peek())) {
            val unaryOperator = read()
            val rand = unary()
            unaryOperator match {
                case Some(tok) => Unary(unaryOperatorType(tok.typ), rand, Some(tok))
                case _ => throw ParserException(peek(), "Expect unary op")
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

    def primary(): Expr = {
        read() match {
            case Some(tok) =>
                tok.typ match {
                    case TokenType.Number => Number(tok.literal.get.asInstanceOf[Float], Some(tok))
                    case TokenType.Str => Str(tok.lexeme, Some(tok))
                    case TokenType.False => False(Some(tok))
                    case TokenType.True => True(Some(tok))
                    case TokenType.NIL => NIL(Some(tok))
                        // TODO pass this token to the grouping
                    case TokenType.LeftParen => grouping()
                }
            case _ => throw ParserException(peek(), "Expect expression")
        }
    }

    // Assume the left paren was read by the caller
    def grouping(): Expr = {
        val inner = expression()
        read() match {
            case Some(tok) if tok.typ == TokenType.RightParen =>
                Grouping(inner, Some(tok))
            case _ => throw ParserException(peek(), "Expect closing paren")
        }
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