package com.zackmarvel.slox

import org.scalatest._

class InterpreterSpec extends FlatSpec with Matchers {
    "An interpreter" should "identify equal booleans as ==" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(False(None), equal = true, False(None)) shouldBe true
        interpreter.evalEquality(True(None), equal = true, True(None)) shouldBe true
        interpreter.evalEquality(False(None), equal = false, False(None)) shouldBe false
        interpreter.evalEquality(True(None), equal = false, True(None)) shouldBe false
    }

    it should "identify non-equal booleans as !=" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(False(None), equal = true, True(None)) shouldBe false
        interpreter.evalEquality(True(None), equal = true, False(None)) shouldBe false
        interpreter.evalEquality(False(None), equal = false, True(None)) shouldBe true
        interpreter.evalEquality(True(None), equal = false, False(None)) shouldBe true
    }

    it should "identify equal numbers as ==" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Number(1.2f, None), equal = true, Number(1.2f, None)) shouldBe true
        interpreter.evalEquality(Number(1.2f, None), equal = false, Number(1.2f, None)) shouldBe false
    }

    it should "identify non-equal numbers as !=" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Number(1.2f, None), equal = true, Number(2.2f, None)) shouldBe false
        interpreter.evalEquality(Number(1.2f, None), equal = false, Number(2.2f, None)) shouldBe true
    }

    it should "identify equal strings as ==" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Str("asdf", None), equal = true, Str("asdf", None)) shouldBe true
        interpreter.evalEquality(Str("asdf", None), equal = false, Str("asdf", None)) shouldBe false
    }

    it should "identify non-equal strings as !=" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Str("asdf", None), equal = true, Str("sdfg", None)) shouldBe false
        interpreter.evalEquality(Str("asdf", None), equal = false, Str("sdfg", None)) shouldBe true
    }

    it should "add two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalAddition(Number(20.0f, None), AdditionType.Plus, Number(22.0f, None)) shouldBe 42.0f
    }

    it should "subtract two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalAddition(Number(62.0f, None), AdditionType.Minus, Number(20.0f, None)) shouldBe 42.0f
    }

    it should "fail to add a number to a boolean" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalAddition(Number(62.0f, None), AdditionType.Plus, False(None))
    }

    it should "fail to add two booleans" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalAddition(True(None), AdditionType.Plus, False(None))
    }

    it should "add two strings" in {
        val interpreter = new Interpreter
        interpreter.evalAddition(Str("a", None), AdditionType.Plus, Str("b", None)) shouldBe "ab"
    }

    it should "multiply two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalMultiplication(Number(21.0f, None), MultiplicationType.Star,
            Number(2.0f, None)) shouldBe 42.0f
    }

    it should "divide two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalMultiplication(Number(84.0f, None), MultiplicationType.Slash,
            Number(2.0f, None)) shouldBe 42.0f
    }

    it should "fail to multiply two booleans" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            False(None), MultiplicationType.Star, False(None))
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            True(None), MultiplicationType.Star, False(None))
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            True(None), MultiplicationType.Star, True(None))
    }

    it should "fail to multiply two strings" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            Str("asdf", None), MultiplicationType.Star, Str("sdfg", None))
    }

    it should "negate numbers" in {
        val interpreter = new Interpreter
        interpreter.evalUnary(UnaryType.Minus, Number(42.0f, None)) shouldBe (-42.0f)
    }

    it should "fail to negate booleans" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Minus, False(None))
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Minus, True(None))
    }

    it should "fail to negate strings" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Minus, Str("", None))
    }

    it should "logically invert (!) booleans " in {
        val interpreter = new Interpreter
        interpreter.evalUnary(UnaryType.Bang, False(None)) shouldBe true
        interpreter.evalUnary(UnaryType.Bang, True(None)) shouldBe false
    }

    it should "fail to logically invert numbers" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Bang, Number(42.0f, None))
    }

    it should "fail to logically invert strings" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Bang, Str("asdf", None))
    }

    it should "evaluate nil" in {
        val interpreter = new Interpreter
        interpreter.eval(NIL(None)) shouldBe Nil
    }

    it should "evaluate complex expressions" in {
        val interpreter = new Interpreter
        // 40+2 != 88/2-4 should be true (42 != 40)
        interpreter.eval(Equality(
            Addition(Number(40.0f, None), AdditionType.Plus, Number(2.0f, None), None),
            equal = false,
            Addition(
                Multiplication(Number(88.0f, None), MultiplicationType.Slash, Number(2.0f, None), None),
                AdditionType.Minus,
                Number(4.0f, None),
                None
            ),
            None
        )) shouldBe true
    }

}