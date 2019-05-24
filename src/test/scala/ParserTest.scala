package com.zackmarvel.slox;

import org.scalatest._;
import org.scalatest.OptionValues._;

class ParserSpec extends FlatSpec with Matchers {
    "A parser" should "parse number tokens" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "1234.0", Some(1234.0f), 1)
        ))
        parser.primary() should be (Number(1234.0f))
    }

    it should "parse string tokens" in {
        val parser = new Parser(Array(
            Token(TokenType.Str, "asdf asdf", Some("asdf asdf"), 1)
        ))
        parser.primary() should be (Str("asdf asdf"))
    }

    it should "parse true tokens" in {
        val parser = new Parser(Array(
            Token(TokenType.True, "true", None, 1)
        ))
        parser.primary() should be (True)
    }

    it should "parse false tokens" in {
        val parser = new Parser(Array(
            Token(TokenType.False, "false", None, 1)
        ))
        parser.primary() should be (False)
    }

    it should "parse nil tokens" in {
        val parser = new Parser(Array(
            Token(TokenType.NIL, "nil", None, 1)
        ))
        parser.primary() should be (NIL)
    }

    it should "parse (grouped tokens)" in {
        val parser = new Parser(Array(
            Token(TokenType.LeftParen, "(", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
            Token(TokenType.RightParen, ")", None, 1),
        ))
        parser.primary() should be (Grouping(Number(123.0f)))
    }

    it should "parse ((grouped tokens))" in {
        val parser = new Parser(Array(
            Token(TokenType.LeftParen, "(", None, 1),
            Token(TokenType.LeftParen, "(", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
            Token(TokenType.RightParen, ")", None, 1),
            Token(TokenType.RightParen, ")", None, 1),
        ))
        parser.primary() should be (Grouping(Grouping(Number(123.0f))))
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
        val parser = new Parser(Array(
            Token(TokenType.Minus, "-", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
        ))
        parser.unary() should be (Unary(UnaryType.Minus, Number(123.0f)))
    }

    it should "parse nested unary operators" in {
        val parser = new Parser(Array(
            Token(TokenType.Bang, "!", None, 1),
            Token(TokenType.Minus, "-", None, 1),
            Token(TokenType.Number, "123.0", Some(123.0f), 1),
        ))
        parser.unary() should be (Unary(UnaryType.Bang, Unary(UnaryType.Minus, Number(123.0f))))
    }

    it should "parse multiplcation" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "21.0", Some(21.0f), 1),
            Token(TokenType.Star, "*", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.multiplication() should be (Multiplication(
            Number(21.0f),
            MultiplicationType.Star,
            Number(2.0f)
        ))
    }

    it should "parse division" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "84.0", Some(84.0f), 1),
            Token(TokenType.Slash, "/", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.multiplication() should be (Multiplication(
            Number(84.0f),
            MultiplicationType.Slash,
            Number(2.0f)
        ))
    }

    it should "parse addition" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "40.0", Some(40.0f), 1),
            Token(TokenType.Plus, "+", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.addition() should be (Addition(
            Number(40.0f),
            AdditionType.Plus,
            Number(2.0f)
        ))
    }

    it should "parse subtraction" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.Minus, "-", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.addition() should be (Addition(
            Number(44.0f),
            AdditionType.Minus,
            Number(2.0f)
        ))
    }

    it should "parse greater than" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.Greater, ">", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.comparison() should be (Comparison(
            Number(44.0f),
            ComparisonType.Greater,
            Number(2.0f)
        ))
    }

    it should "parse greater than or equal" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.GreaterEqual, ">=", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.comparison() should be (Comparison(
            Number(44.0f),
            ComparisonType.GreaterEqual,
            Number(2.0f)
        ))
    }

    it should "parse less than" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.Less, "<", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.comparison() should be (Comparison(
            Number(44.0f),
            ComparisonType.Less,
            Number(2.0f)
        ))
    }

    it should "parse less than or equal" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.LessEqual, "<=", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.comparison() should be (Comparison(
            Number(44.0f),
            ComparisonType.LessEqual,
            Number(2.0f)
        ))
    }

    it should "parse equal" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.EqualEqual, "==", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.equality() should be (Equality(
            Number(44.0f),
            true,
            Number(2.0f)
        ))
    }

    it should "parse not equal" in {
        val parser = new Parser(Array(
            Token(TokenType.Number, "44.0", Some(44.0f), 1),
            Token(TokenType.BangEqual, "!=", None, 1),
            Token(TokenType.Number, "2.0", Some(2.0f), 1),
        ))
        parser.equality() should be (Equality(
            Number(44.0f),
            false,
            Number(2.0f)
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
                Number(40.0f),
                AdditionType.Plus,
                Number(2.0f)
            ),
            false,
            Addition(
                Multiplication(
                    Number(88.0f),
                    MultiplicationType.Slash,
                    Number(2.0f),
                ),
                AdditionType.Minus,
                Number(4.0f)
            )
        ))
    }

}