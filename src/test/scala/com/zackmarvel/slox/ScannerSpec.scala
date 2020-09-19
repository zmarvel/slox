package com.zackmarvel.slox;

import org.scalatest.OptionValues._
import org.scalatest._;

class ScannerSpec extends FlatSpec with Matchers {
    "A Scanner" should "parse ( as LeftParen" in {
        val scanner = new Scanner("(")
        scanner.scanToken().value.typ should be (TokenType.LeftParen)
        scanner.pos should be (1)
    }

    it should "parse ) as RightParen" in {
        val scanner = new Scanner(")")
        scanner.scanToken().value.typ should be (TokenType.RightParen)
        scanner.pos should be (1)
    }

    it should "parse { as LeftBrace" in {
        val scanner = new Scanner("{")
        scanner.scanToken().value.typ should be (TokenType.LeftBrace)
        scanner.pos should be (1)
    }

    it should "parse } as RightBrace" in {
        val scanner = new Scanner("}")
        scanner.scanToken().value.typ should be (TokenType.RightBrace)
        scanner.pos should be (1)
    }

    it should "parse , as Comma" in {
        val scanner = new Scanner(",")
        scanner.scanToken().value.typ should be (TokenType.Comma)
        scanner.pos should be (1)
    }

    it should "parse . as Dot" in {
        val scanner = new Scanner(".")
        scanner.scanToken().value.typ should be (TokenType.Dot)
        scanner.pos should be (1)
    }

    it should "parse ; as Semicolon" in {
        val scanner = new Scanner(";")
        scanner.scanToken().value.typ should be (TokenType.Semicolon)
        scanner.pos should be (1)
    }

    it should "parse - as Minus" in {
        val scanner = new Scanner("-")
        scanner.scanToken().value.typ should be (TokenType.Minus)
        scanner.pos should be (1)
    }

    it should "parse + as Plus" in {
        val scanner = new Scanner("+")
        scanner.scanToken().value.typ should be (TokenType.Plus)
        scanner.pos should be (1)
    }

    it should "parse ! as Bang" in {
        val scanner = new Scanner("!")
        scanner.scanToken().value.typ should be (TokenType.Bang)
        scanner.pos should be (1)
    }

    it should "parse != as BangEqual" in {
        val scanner = new Scanner("!=")
        scanner.scanToken().value.typ should be (TokenType.BangEqual)
        scanner.pos should be (2)
    }

    it should "parse = as Equal" in {
        val scanner = new Scanner("=")
        scanner.scanToken().value.typ should be (TokenType.Equal)
        scanner.pos should be (1)
    }

    it should "parse == as EqualEqual" in {
        val scanner = new Scanner("==")
        scanner.scanToken().value.typ should be (TokenType.EqualEqual)
        scanner.pos should be (2)
    }

    it should "parse > as Greater" in {
        val scanner = new Scanner(">")
        scanner.scanToken().value.typ should be (TokenType.Greater)
        scanner.pos should be (1)
    }

    it should "parse >= as GreaterEqual" in {
        val scanner = new Scanner(">=")
        scanner.scanToken().value.typ should be (TokenType.GreaterEqual)
        scanner.pos should be (2)
    }

    it should "parse < as Less" in {
        val scanner = new Scanner("<")
        scanner.scanToken().value.typ should be (TokenType.Less)
        scanner.pos should be (1)
    }

    it should "parse <= as LessEqual" in {
        val scanner = new Scanner("<=")
        scanner.scanToken().value.typ should be (TokenType.LessEqual)
        scanner.pos should be (2)
    }

    it should "skip whitespace" in {
        val scanner = new Scanner(" \t\r\n")
        // read the space
        scanner.scanToken() should be (None)
        scanner.pos should be (1)
        // read the tab
        scanner.scanToken() should be (None)
        scanner.pos should be (2)
        // read the carriage return
        scanner.scanToken() should be (None)
        scanner.pos should be (3)
        // read the newline
        scanner.scanToken() should be (None)
        scanner.line should be (2)
        scanner.pos should be (4)
    }

    it should "skip comments" in {
        val scanner = new Scanner(" // this is a comment\n")
        // read the leading space
        scanner.scanToken() should be (None)
        scanner.pos should be (1)
        // read the comment
        scanner.scanToken() should be (None)
        scanner.pos should be (21)
        // read the newline
        scanner.scanToken() should be (None)
        scanner.line should be (2)
        scanner.pos should be (22)
    }

    it should "parse empty strings" in {
        val scanner = new Scanner("\"\"")
        val tok = scanner.scanToken().value
        tok.typ should be (TokenType.Str)
        tok.lexeme should be ("")
        tok.literal.value should be ("")
        scanner.pos should be (2)
    }

    it should "parse strings between matching quotations" in {
        val scanner = new Scanner("\" I am a string\"")
        val tok = scanner.scanToken().value
        tok.typ should be (TokenType.Str)
        tok.lexeme should be (" I am a string")
        tok.literal.value should be (" I am a string")
        scanner.pos should be (16)
    }

    it should "fail to parse unmatched quotations" in {
        val scanner = new Scanner("\"")
        scanner.scanToken() should be (None)
        scanner.pos should be (1)
    }

    it should "fail to parse unterminated strings" in {
        val scanner = new Scanner("\"aaa")
        scanner.scanToken() should be (None)
        scanner.pos should be (4)
    }

    it should "parse whole numbers" in {
        val scanner = new Scanner("1234")
        val tok = scanner.scanToken().value
        tok.typ should be (TokenType.Number)
        tok.lexeme should be ("1234.0")
        tok.literal.value should be (1234.0)
        scanner.pos should be (4)
    }

    // TODO: can we catch this in the scanner?
    it should "parse numbers with leading decimal points" in {
        val scanner: Scanner = new Scanner(".1234")
        val tok1 = scanner.scanToken().value
        tok1 should be (Token(TokenType.Dot, ".", None, 1))
        val tok2 = scanner.scanToken().value
        tok2 should be (Token(TokenType.Number, "1234.0", Some(1234.0), 1))
    }
    // it should "fail to parse numbers with leading decimal points" in {
        // val scanner = new Scanner(".1234")
        // val tok = scanner.scanToken() should be (None)
    // }

    it should "fail to parse numbers with trailing decimal points" in {
        val scanner = new Scanner("1234.")
        scanner.scanToken() should be (None)
    }

    it should "parse reserved keywords" in {
        {
            val scanner = new Scanner("and")
            scanner.scanToken().value should be (Token(TokenType.And, "and", None, 1))
        }
        {
            val scanner = new Scanner("class")
            scanner.scanToken().value should be (Token(TokenType.Class, "class", None, 1))
        }
        {
            val scanner = new Scanner("else")
            scanner.scanToken().value should be (Token(TokenType.Else, "else", None, 1))
        }
        {
            val scanner = new Scanner("false")
            scanner.scanToken().value should be (Token(TokenType.False, "false", None, 1))
        }
        {
            val scanner = new Scanner("for")
            scanner.scanToken().value should be (Token(TokenType.For, "for", None, 1))
        }
        {
            val scanner = new Scanner("fun")
            scanner.scanToken().value should be (Token(TokenType.Fun, "fun", None, 1))
        }
        {
            val scanner = new Scanner("if")
            scanner.scanToken().value should be (Token(TokenType.If, "if", None, 1))
        }
        {
            val scanner = new Scanner("nil")
            scanner.scanToken().value should be (Token(TokenType.NIL, "nil", None, 1))
        }
        {
            val scanner = new Scanner("or")
            scanner.scanToken().value should be (Token(TokenType.Or, "or", None, 1))
        }
        {
            val scanner = new Scanner("print")
            scanner.scanToken().value should be (Token(TokenType.Print, "print", None, 1))
        }
        {
            val scanner = new Scanner("return")
            scanner.scanToken().value should be (Token(TokenType.Return, "return", None, 1))
        }
        {
            val scanner = new Scanner("super")
            scanner.scanToken().value should be (Token(TokenType.Super, "super", None, 1))
        }
        {
            val scanner = new Scanner("this")
            scanner.scanToken().value should be (Token(TokenType.This, "this", None, 1))
        }
        {
            val scanner = new Scanner("true")
            scanner.scanToken().value should be (Token(TokenType.True, "true", None, 1))
        }
        {
            val scanner = new Scanner("var")
            scanner.scanToken().value should be (Token(TokenType.Var, "var", None, 1))
        }
        {
            val scanner = new Scanner("while")
            scanner.scanToken().value should be (Token(TokenType.While, "while", None, 1))
        }
    }

    it should "parse identifiers" in {
        val scanner = new Scanner("xyz")
        val tokens = scanner.scanTokens()
        tokens(0) should be (Token(TokenType.Identifier, "xyz", None, 1))
        tokens(1) should be (Token(TokenType.Eof, "", None,  1))
    }
}