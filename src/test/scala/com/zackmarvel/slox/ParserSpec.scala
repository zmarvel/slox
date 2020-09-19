package com.zackmarvel.slox;

import org.scalatest._;

class ParserSpec extends FlatSpec with Matchers {
    "A parser" should "parse number tokens" in {
        val tok = Token(TokenType.Number, "1234.0", Some(1234.0f), 1)
        val parser = new Parser(Array(tok))
        parser.primary() shouldBe Number(1234.0f, Some(tok))
    }

    it should "parse string tokens" in {
        val tok = Token(TokenType.Str, "asdf asdf", Some("asdf asdf"), 1)
        val parser = new Parser(Array(tok))
        parser.primary() shouldBe Str("asdf asdf", Some(tok))
    }

    it should "parse true tokens" in {
        val tok = Token(TokenType.True, "true", None, 1)
        val parser = new Parser(Array(tok))
        parser.primary() shouldBe True(Some(tok))
    }

    it should "parse false tokens" in {
        val tok = Token(TokenType.False, "false", None, 1)
        val parser = new Parser(Array(tok))
        parser.primary() shouldBe False(Some(tok))
    }

    it should "parse nil tokens" in {
        val tok = Token(TokenType.NIL, "nil", None, 1)
        val parser = new Parser(Array(tok))
        parser.primary() shouldBe NIL(Some(tok))
    }

    it should "parse (grouped tokens)" in {
        val tokens = Array(
            Token(TokenType.LeftParen, "(", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
            Token(TokenType.RightParen, ")", None, 1),
        )
        val parser = new Parser(tokens)
        parser.primary() shouldBe Grouping(Number(123.0f, Some(tokens(1))), Some(tokens(2)))
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
        parser.primary() should be (Grouping(Grouping(Number(123.0f, Some(tokens(2))), Some(tokens(3))), Some(tokens(4))))
    }

    it should "not parse unmatched parens" in {
        val parser = new Parser(Array(
            Token(TokenType.LeftParen, "(", None, 1),
            Token(TokenType.LeftParen, "(", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
            Token(TokenType.RightParen, ")", None, 1),
        ))
        // TODO throw a parser exception
        a [ParserException] should be thrownBy parser.primary()
    }

    it should "parse unary operators" in {
      val tokens = Array(
            Token(TokenType.Minus, "-", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.unary() shouldBe Unary(UnaryType.Minus, Number(123.0f, Some(tokens(1))), Some(tokens(0)))
    }

    it should "parse nested unary operators" in {
        val tokens = Array(
            Token(TokenType.Bang, "!", None, 1),
            Token(TokenType.Minus, "-", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.unary() shouldBe Unary(
            UnaryType.Bang,
            Unary(UnaryType.Minus, Number(123.0f, Some(tokens(2))), Some(tokens(1))),
            Some(tokens(0)))
    }

    it should "parse multiplcation" in {
        val tokens = Array(
            Token(TokenType.Number, "21.0", Some(21.0f), 1),
            Token(TokenType.Star, "*", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.multiplication() shouldBe Multiplication(
            Number(21.0f, Some(tokens(0))),
            MultiplicationType.Star,
            Number(2.0f, Some(tokens(2))), Some(tokens(1)))
    }

    it should "parse division" in {
      val tokens = Array(
            Token(TokenType.Number, "84.0", Some(84.0f), 1),
            Token(TokenType.Slash, "/", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.multiplication() shouldBe Multiplication(
            Number(84.0f, Some(tokens(0))),
            MultiplicationType.Slash,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse addition" in {
      val tokens = Array(
            Token(TokenType.Number, "40.0", Some(40.0f), 1),
            Token(TokenType.Plus, "+", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.addition() shouldBe Addition(
            Number(40.0f, Some(tokens(0))),
            AdditionType.Plus,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse subtraction" in {
      val tokens = Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.Minus, "-", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.addition() shouldBe Addition(
            Number(44.0f, Some(tokens(0))),
            AdditionType.Minus,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse greater than" in {
      val tokens = Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.Greater, ">", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.comparison() shouldBe Comparison(
            Number(44.0f, Some(tokens(0))),
            ComparisonType.Greater,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse greater than or equal" in {
        val tokens = Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.GreaterEqual, ">=", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.comparison() shouldBe Comparison(
            Number(44.0f, Some(tokens(0))),
            ComparisonType.GreaterEqual,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse less than" in {
        val tokens = Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.Less, "<", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.comparison() shouldBe Comparison(
            Number(44.0f, Some(tokens(0))),
            ComparisonType.Less,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse less than or equal" in {
        val tokens = Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.LessEqual, "<=", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.comparison() shouldBe Comparison(
            Number(44.0f, Some(tokens(0))),
            ComparisonType.LessEqual,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse equal" in {
        val tokens = Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.EqualEqual, "==", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        )
        val parser = new Parser(tokens)
        parser.equality() shouldBe Equality(
            Number(44.0f, Some(tokens(0))),
            true,
            Number(2.0f, Some(tokens(2))),
            Some(tokens(1))
        )
    }

    it should "parse not equal" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.BangEqual, "!=", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.equality() should be (Equality(
            Number(44.0f, Some(parser.tokens(0))),
            false,
            Number(2.0f, Some(parser.tokens(2))),
            Some(parser.tokens(1))
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
        parser.expression() should be (Equality(
            Addition(
                Number(40.0f, Some(parser.tokens(0))),
                AdditionType.Plus,
                Number(2.0f, Some(parser.tokens(2))),
                Some(parser.tokens(1))
            ),
            false,
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

}