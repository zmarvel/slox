package com.zackmarvel.slox

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "A parser" should "parse number tokens" in {
    val tok = Token(TokenType.Number, "1234.0", Some(1234.0f), 1)
    val parser = new Parser(Array(tok))
    parser.primary() shouldBe Left(Number(1234.0f, Some(tok)))
  }

  it should "parse string tokens" in {
    val tok = Token(TokenType.Str, "asdf asdf", Some("asdf asdf"), 1)
    val parser = new Parser(Array(tok))
    parser.primary() shouldBe Left(Str("asdf asdf", Some(tok)))
  }

  it should "parse true tokens" in {
    val tok = Token(TokenType.True, "true", None, 1)
    val parser = new Parser(Array(tok))
    parser.primary() shouldBe Left(True(Some(tok)))
  }

  it should "parse false tokens" in {
    val tok = Token(TokenType.False, "false", None, 1)
    val parser = new Parser(Array(tok))
    parser.primary() shouldBe Left(False(Some(tok)))
  }

  it should "parse nil tokens" in {
    val tok = Token(TokenType.NIL, "nil", None, 1)
    val parser = new Parser(Array(tok))
    parser.primary() shouldBe Left(NIL(Some(tok)))
  }

  it should "parse (grouped tokens)" in {
    val tokens = Array(
      Token(TokenType.LeftParen, "(", None, 1),
      Token(TokenType.Number, "123.0", Some(123.0f), 1),
      Token(TokenType.RightParen, ")", None, 1),
    )
    val parser = new Parser(tokens)
    parser.primary() shouldBe Left(Grouping(Number(123.0f, Some(tokens(1))), Some(tokens(2))))
  }

  it should "parse ((grouped tokens))" in {
    val tokens = Array(
      Token(TokenType.LeftParen, "(", None, 1),
      Token(TokenType.LeftParen, "(", None, 1),
      Token(TokenType.Number, "123.0", Some(123.0f), 1),
      Token(TokenType.RightParen, ")", None, 1),
      Token(TokenType.RightParen, ")", None, 1),
    )
    val parser = new Parser(tokens)
    parser.primary() shouldBe Left(Grouping(Grouping(Number(123.0f, Some(tokens(2))), Some(tokens(3))), Some(tokens(4))))
  }

  it should "not parse unmatched parens" in {
    val parser = new Parser(Array(
      Token(TokenType.LeftParen, "(", None, 1),
      Token(TokenType.LeftParen, "(", None, 1),
      Token(TokenType.Number, "123.0", Some(123.0f), 1),
      Token(TokenType.RightParen, ")", None, 1),
    ))
    parser.primary() shouldBe Right(ParseError(None, "Expect closing paren"))
  }

  it should "parse unary operators" in {
    val tokens = Array(
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "123.0", Some(123.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.unary() shouldBe Left(Unary(UnaryType.Minus, Number(123.0f, Some(tokens(1))), Some(tokens(0))))
  }

  it should "parse nested unary operators" in {
    val tokens = Array(
      Token(TokenType.Bang, "!", None, 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "123.0", Some(123.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.unary() shouldBe Left(Unary(
      UnaryType.Bang,
      Unary(UnaryType.Minus, Number(123.0f, Some(tokens(2))), Some(tokens(1))),
      Some(tokens(0))))
  }

  it should "parse multiplcation" in {
    val tokens = Array(
      Token(TokenType.Number, "21.0", Some(21.0f), 1),
      Token(TokenType.Star, "*", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.multiplication() shouldBe Left(Multiplication(
      Number(21.0f, Some(tokens(0))),
      MultiplicationType.Star,
      Number(2.0f, Some(tokens(2))), Some(tokens(1))))
  }

  it should "parse division" in {
    val tokens = Array(
      Token(TokenType.Number, "84.0", Some(84.0f), 1),
      Token(TokenType.Slash, "/", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.multiplication() shouldBe Left(Multiplication(
      Number(84.0f, Some(tokens(0))),
      MultiplicationType.Slash,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse addition" in {
    val tokens = Array(
      Token(TokenType.Number, "40.0", Some(40.0f), 1),
      Token(TokenType.Plus, "+", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.addition() shouldBe Left(Addition(
      Number(40.0f, Some(tokens(0))),
      AdditionType.Plus,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse subtraction" in {
    val tokens = Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.addition() shouldBe Left(Addition(
      Number(44.0f, Some(tokens(0))),
      AdditionType.Minus,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse greater than" in {
    val tokens = Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.Greater, ">", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.comparison() shouldBe Left(Comparison(
      Number(44.0f, Some(tokens(0))),
      ComparisonType.Greater,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse greater than or equal" in {
    val tokens = Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.GreaterEqual, ">=", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.comparison() shouldBe Left(Comparison(
      Number(44.0f, Some(tokens(0))),
      ComparisonType.GreaterEqual,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse less than" in {
    val tokens = Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.Less, "<", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.comparison() shouldBe Left(Comparison(
      Number(44.0f, Some(tokens(0))),
      ComparisonType.Less,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse less than or equal" in {
    val tokens = Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.LessEqual, "<=", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.comparison() shouldBe Left(Comparison(
      Number(44.0f, Some(tokens(0))),
      ComparisonType.LessEqual,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse equal" in {
    val tokens = Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.EqualEqual, "==", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    )
    val parser = new Parser(tokens)
    parser.equality() shouldBe Left(Equality(
      Number(44.0f, Some(tokens(0))),
      equal = true,
      Number(2.0f, Some(tokens(2))),
      Some(tokens(1))
    ))
  }

  it should "parse not equal" in {
    val parser = new Parser(Array(
      Token(TokenType.Number, "44.0", Some(44.0f), 1),
      Token(TokenType.BangEqual, "!=", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
    ))
    parser.equality() shouldBe Left(Equality(
      Number(44.0f, Some(parser.tokens(0))),
      equal = false,
      right = Number(2.0f, Some(parser.tokens(2))),
      token = Some(parser.tokens(1))
    ))
  }

  it should "parse complex expressions" in {
    val parser = new Parser(Array(
      Token(TokenType.Number, "40.0", Some(40.0f), 1),
      Token(TokenType.Plus, "+", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
      Token(TokenType.BangEqual, "!=", None, 1),
      Token(TokenType.Number, "88.0", Some(88.0f), 1),
      Token(TokenType.Slash, "/", None, 1),
      Token(TokenType.Number, "2.0", Some(2.0f), 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "4.0", Some(4.0f), 1),
    ))
    parser.expression() shouldBe Left(Equality(
      Addition(
        Number(40.0f, Some(parser.tokens(0))),
        AdditionType.Plus,
        Number(2.0f, Some(parser.tokens(2))),
        Some(parser.tokens(1))
      ),
      equal = false,
      Addition(
        Multiplication(
          Number(88.0f, Some(parser.tokens(4))),
          MultiplicationType.Slash,
          Number(2.0f, Some(parser.tokens(6))),
          Some(parser.tokens(5))
        ),
        AdditionType.Minus,
        Number(4.0f, Some(parser.tokens(8))),
        Some(parser.tokens(7))
      ),
      Some(parser.tokens(3))
    ))
  }

  it should "parse a statement containing only a number" in {
    val tokens = Array(
      Token(TokenType.Number, "40.0", Some(40.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Left(ExprStmt(Number(40.0f, Some(tokens(0)))))
  }

  it should "parse a statement containing only a string" in {
    val tokens = Array(
      Token(TokenType.Str, "Howdy", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Left(ExprStmt(Str("Howdy", Some(tokens(0)))))
  }

  it should "parse statements containing booleans" in {
    val tokens = Array(
      Token(TokenType.True, "true", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
      Token(TokenType.False, "false", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Left(ExprStmt(True(Some(tokens(0)))))
    parser.statement() shouldBe Left(ExprStmt(False(Some(tokens(2)))))
  }

  it should "parse statements numeric expressions" in {
    val tokens = Array(
      Token(TokenType.LeftParen, "(", None, 1),
      Token(TokenType.Number, "40.0", Some(40.0f), 1),
      Token(TokenType.Minus, "-", None, 1),
      Token(TokenType.Number, "20.0", Some(20.0f), 1),
      Token(TokenType.RightParen, ")", None, 1),
      Token(TokenType.Slash, "/", None, 1),
      Token(TokenType.Number, "2", Some(2.0f), 1),
      Token(TokenType.Plus, "+", None, 1),
      Token(TokenType.Number, "32.0", Some(32.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Left(ExprStmt(
      Addition(
        Multiplication(
          Grouping(Addition(Number(40.0f, Some(tokens(1))), AdditionType.Minus, Number(20.0f, Some(tokens(3))), Some(tokens(2))), Some(tokens(4))),
          MultiplicationType.Slash,
          Number(2.0f, Some(tokens(6))),
          Some(tokens(5))),
        AdditionType.Plus,
        Number(32.0f, Some(tokens(8))),
        Some(tokens(7)))))
  }

  it should "parse val statements" in {
    val tokens = Array(
      Token(TokenType.Val, "val", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Equal, "=", None, 1),
      Token(TokenType.Number, "42.0", Some(42.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Left(ValDeclaration(tokens(1), Number(42.0f, Some(tokens(3)))))
  }

  it should "not accept val statements with no =" in {
    val tokens = Array(
      Token(TokenType.Val, "val", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Number, "42.0", Some(42.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Right(ParseError(Some(tokens(2)), "Expected `='"))
  }

  it should "not accept val statements with no right hand side" in {
    val tokens = Array(
      Token(TokenType.Val, "val", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Right(ParseError(Some(tokens(2)), "Expected `='"))
  }

  it should "only accept val statements with expressions on the right hand side" in {
    val tokens = Array(
      Token(TokenType.Val, "val", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Equal, "=", None, 1),
      Token(TokenType.Val, "val", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Right(ParseError(Some(tokens(3)), "Expected expression"))
  }

  it should "parse var statements" in {
    val tokens = Array(
      Token(TokenType.Var, "var", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Equal, "=", None, 1),
      Token(TokenType.Number, "22.0", Some(22.0f), 1),
      Token(TokenType.Plus, "+", None, 1),
      Token(TokenType.Number, "20.0", Some(20.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Left(
      VarDeclaration(tokens(1),
        Addition(Number(22.0f, Some(tokens(3))), AdditionType.Plus, Number(20.0f, Some(tokens(5))), Some(tokens(4)))))
  }

  it should "not accept var statements with no =" in {
    val tokens = Array(
      Token(TokenType.Var, "var", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Number, "42.0", Some(42.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Right(ParseError(Some(tokens(2)), "Expected `='"))
  }

  it should "not parse var statements with no right hand side" in {
    val tokens = Array(
      Token(TokenType.Var, "var", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Right(ParseError(Some(tokens(2)), "Expected `='"))
  }

  it should "not parse var statements with invalid identifiers" in {
    val tokens = Array(
      Token(TokenType.Var, "var", None, 1),
      Token(TokenType.Number, "1.0", Some(1.0f), 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.statement() shouldBe Right(ParseError(Some(tokens(1)), "Expected identifier"))
  }

  it should "parse more than one statement in a program" in {
    val tokens = Array(
      Token(TokenType.Val, "val", None, 1),
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Equal, "=", None, 1),
      Token(TokenType.Str, "Asdf", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
      Token(TokenType.Number, "42.0", Some(42.0f), 2),
      Token(TokenType.Semicolon, ";", None, 2)
    )
    val parser = new Parser(tokens)
    parser.program() shouldBe Left(
      Vector(ValDeclaration(tokens(1), Str("Asdf", Some(tokens(3)))),
      ExprStmt(Number(42.0f, Some(tokens(5))))))
  }

  it should "not parse a program that doesn't end with a semicolon" in {
    val tokens = Array(
      Token(TokenType.Number, "42.0", Some(42.0f), 2),
    )
    val parser = new Parser(tokens)
    parser.program() shouldBe Right(ParseError(None, "Expected semicolon following statement"))
  }

  it should "parse identifiers as a primary" in {
    val tokens = Array(
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
      val parser = new Parser(tokens)
      parser.primary() shouldBe Left(Identifier(Some(tokens(0))))
  }

  it should "parse identifiers as an expression" in {
    val tokens = Array(
      Token(TokenType.Identifier, "x", None, 1),
      Token(TokenType.Semicolon, ";", None, 1),
    )
    val parser = new Parser(tokens)
    parser.expression() shouldBe Left(Identifier(Some(tokens(0))))
  }

  it should "parse identifiers in various expressions" in {
    val tokens = Array(
      Token(TokenType.Identifier, "x1", None, 1),
      Token(TokenType.Plus, "+", None, 1),
      Token(TokenType.Identifier, "x2", None, 1),
      Token(TokenType.Star, "*", None, 1),
      Token(TokenType.Identifier, "y", None, 1),
    )
    val parser = new Parser(tokens)
    parser.expression() shouldBe Left(Addition(
      Identifier(Some(tokens(0))),
      AdditionType.Plus,
      Multiplication(
        Identifier(Some(tokens(2))),
        MultiplicationType.Star,
        Identifier(Some(tokens(4))),
        Some(tokens(3))
      ),
      Some(tokens(1))))
  }

}