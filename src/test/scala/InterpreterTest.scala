package com.zackmarvel.slox;

import org.scalatest._;
import org.scalatest.OptionValues._;

class InterpreterSpec extends FlatSpec with Matchers {
    "An interpreter" should "identify equal booleans as ==" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(False, true, False) should be (true)
        interpreter.evalEquality(True, true, True) should be (true)
        interpreter.evalEquality(False, false, False) should be (false)
        interpreter.evalEquality(True, false, True) should be (false)
    }

    it should "identify non-equal booleans as !=" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(False, true, True) should be (false)
        interpreter.evalEquality(True, true, False) should be (false)
        interpreter.evalEquality(False, false, True) should be (true)
        interpreter.evalEquality(True, false, False) should be (true)
    }

    it should "identify equal numbers as ==" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Number(1.2f), true, Number(1.2f)) should be (true)
        interpreter.evalEquality(Number(1.2f), false, Number(1.2f)) should be (false)
    }

    it should "identify non-equal numbers as !=" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Number(1.2f), true, Number(2.2f)) should be (false)
        interpreter.evalEquality(Number(1.2f), false, Number(2.2f)) should be (true)
    }

    it should "identify equal strings as ==" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Str("asdf"), true, Str("asdf")) should be (true)
        interpreter.evalEquality(Str("asdf"), false, Str("asdf")) should be (false)
    }

    it should "identify non-equal strings as !=" in {
        val interpreter = new Interpreter
        interpreter.evalEquality(Str("asdf"), true, Str("sdfg")) should be (false)
        interpreter.evalEquality(Str("asdf"), false, Str("sdfg")) should be (true)
    }

    it should "add two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalAddition(Number(20.0f), AdditionType.Plus, Number(22.0f)) should be (42.0f)
    }

    it should "subtract two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalAddition(Number(62.0f), AdditionType.Minus, Number(20.0f)) should be (42.0f)
    }

    it should "fail to add a number to a boolean" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalAddition(Number(62.0f), AdditionType.Plus, False)
    }

    it should "fail to add two booleans" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalAddition(True, AdditionType.Plus, False)
    }

    it should "add two strings" in {
        val interpreter = new Interpreter
        interpreter.evalAddition(Str("a"), AdditionType.Plus, Str("b")) should be ("ab")
    }

    it should "multiply two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalMultiplication(Number(21.0f), MultiplicationType.Star,
            Number(2.0f)) should be (42.0f)
    }

    it should "divide two numbers" in {
        val interpreter = new Interpreter
        interpreter.evalMultiplication(Number(84.0f), MultiplicationType.Slash,
            Number(2.0f)) should be (42.0f)
    }

    it should "fail to multiply two booleans" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            False, MultiplicationType.Star, False)
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            True, MultiplicationType.Star, False)
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            True, MultiplicationType.Star, True)
    }

    it should "fail to multiply two strings" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalMultiplication(
            Str("asdf"), MultiplicationType.Star, Str("sdfg"))
    }

    it should "negate numbers" in {
        val interpreter = new Interpreter
        interpreter.evalUnary(UnaryType.Minus, Number(42.0f)) should be (-42.0f)
    }

    it should "fail to negate booleans" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Minus, False)
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Minus, True)
    }

    it should "fail to negate strings" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Minus, Str(""))
    }

    it should "logically invert (!) booleans " in {
        val interpreter = new Interpreter
        interpreter.evalUnary(UnaryType.Bang, False) shouldBe (true)
        interpreter.evalUnary(UnaryType.Bang, True) shouldBe (false)
    }

    it should "fail to logically invert numbers" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Bang, Number(42.0f))
    }

    it should "fail to logically invert strings" in {
        val interpreter = new Interpreter
        an [InterpreterException] should be thrownBy interpreter.evalUnary(UnaryType.Bang, Str("asdf"))
    }

    it should "evaluate nil" in {
        val interpreter = new Interpreter
        interpreter.eval(NIL) should be (Nil)
    }

    it should "evaluate complex expressions" in {
        val interpreter = new Interpreter
        // 40+2 != 88/2-4 should be true (42 != 40)
        interpreter.eval(Equality(
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
        )) shouldBe (true)
    }

}