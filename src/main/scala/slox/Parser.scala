package com.zackmarvel.slox;

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
        if (isEof(peek)) {
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
        peek match {
            case Some(Token(TokenType.Print, _, _, _)) => {
                read()
                printStmt()
            }
            case _ => exprStmt()
        }
    }

    def exprStmt(): Stmt = {
        val expr = expression()
        read() match {
            case Some(Token(TokenType.Semicolon, _, _, _)) => ExprStmt(expr)
            case maybeTok => throw new ParserException(maybeTok, "Expected semicolon following statement")
        }
    }

    def printStmt(): Stmt = {
        // Assume the Print token has already been read
        val expr = expression()
        read() match {
            case Some(Token(TokenType.Semicolon, _, _, _)) => Print(expr)
            case maybeTok => throw new ParserException(maybeTok, "Expected semicolon following statement")
        }
    }

    def expression(): Expr = {
        equality()
    }

    def equality(): Expr = {
        var left = comparison()
        while (isEqualityOperator(peek)) {
            val equalityOperator = read()
            val right = comparison()
            equalityOperator match {
                case Some(Token(TokenType.EqualEqual, _, _, _)) => {
                    left = Equality(left, true, right)
                }
                case Some(Token(_, _, _, _)) => {
                    left = Equality(left, false, right)
                }
                case _ => throw new ParserException(peek, "Expect equality op")
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

    val comparisonOperatorType = Map(
        TokenType.Greater -> ComparisonType.Greater,
        TokenType.GreaterEqual -> ComparisonType.GreaterEqual,
        TokenType.Less -> ComparisonType.Less,
        TokenType.LessEqual -> ComparisonType.LessEqual,
    )

    def comparison(): Expr = {
        var left = addition()
        while (isComparisonOperator(peek)) {
            val comparisonOperator = read()
            val right = addition()
            comparisonOperator match {
                case Some(Token(typ, _, _, _)) => {
                    left = Comparison(left, comparisonOperatorType(typ), right)
                }
                case _ => throw new ParserException(peek, "Expect comparison op")
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

    val additionOperatorType = Map(
        TokenType.Minus -> AdditionType.Minus,
        TokenType.Plus -> AdditionType.Plus,
    )

    def addition(): Expr = {
        var left = multiplication()
        while (isAdditionOperator(peek)) {
            val additionOperator = read
            val right = multiplication()
            additionOperator match {
                case Some(Token(typ, _, _, _)) => {
                    left = Addition(left, additionOperatorType(typ), right)
                }
                case _ => throw new ParserException(peek, "Expect addition op")
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

    val multOperatorType = Map(
        TokenType.Star -> MultiplicationType.Star,
        TokenType.Slash -> MultiplicationType.Slash,
    )

    def multiplication(): Expr = {
        var left = unary()
        while (isMultOperator(peek)) {
            val multOperator = read()
            val right = unary()
            multOperator match {
                case Some(Token(typ, _, _, _)) => {
                    left = Multiplication(left, multOperatorType(typ), right)
                }
                case _ => throw new ParserException(peek, "Expect multiplication op")
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

    val unaryOperatorType = Map(
        TokenType.Bang -> UnaryType.Bang,
        TokenType.Minus -> UnaryType.Minus,
    )

    def unary(): Expr = {
        if (isUnaryOperator(peek)) {
            val unaryOperator = read()
            val rand = unary()
            unaryOperator match {
                case Some(Token(typ, _, _, _)) => Unary(unaryOperatorType(typ), rand)
                case _ => throw new ParserException(peek, "Expect unary op")
            }
        } else {
            primary()
        }
    }

    def isUnaryOperator(tok: Option[Token]): Boolean = {
        tok match {
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
            case Some(Token(TokenType.Number, _, x, _)) => Number(x.get.asInstanceOf[Float])
            case Some(Token(TokenType.Str, x, _, _)) => Str(x)
            case Some(Token(TokenType.False, _, _, _)) => False
            case Some(Token(TokenType.True, _, _, _)) => True
            case Some(Token(TokenType.NIL, _, _, _)) => NIL
            case Some(Token(TokenType.LeftParen, _, _, _)) => grouping()
            case _ => throw new ParserException(peek, "Expect expression")
        }
    }

    // Assume the left paren was read by the caller
    def grouping(): Expr = {
        val inner = expression()
        read() match {
            case Some(Token(TokenType.RightParen, _, _, _)) => {
                Grouping(inner)
            }
            case _ => throw new ParserException(peek, "Expect closing paren")
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