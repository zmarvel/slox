package com.zackmarvel.slox;

import java.io.File;

object Lox {
    val USAGE = "USAGE: slox [script]"
    var sawError = false

    def main(args: Array[String]): Unit = {
        if (args.length > 1) {
            println(USAGE)
            System.exit(64)
        } else if (args.length == 1) {
            runFile(args(0))
        } else {
            runPrompt()
        }
    }

    def runFile(path: String): Unit = {
        val source = io.Source.fromFile(new File(path))
        for (line <- source.getLines()) {
            run(line)
            if (sawError) {
                System.exit(65)
            }
        }
    }

    def runPrompt(): Unit = {
        while (true) {
            run(io.StdIn.readLine("> "))
            sawError = false
        }
    }

    def run(line: String): Unit = {
        val tokens = new Scanner(line).scanTokens()
        val parser = new Parser(tokens.toArray)
        parser.parse() match {
            case Left(expr) => println(expr)
            case Right(err) =>
                err.tok match {
                    case Some(tok) => report(Some(tok.line), "", err.msg)
                    case _ => report(None, "", err.msg)
                }
        }
    }

    def error(line: Option[Int], message: String): Unit = {
        report(line, "", message)
    }

    def report(line: Option[Int], where: String, message: String): Unit = {
        line match {
            case Some(n) => Console.err.println(s"[line $n] Error$where: $message")
            case _ => Console.err.println(s"[line ??] Error$where: $message")
        }
        sawError = true
    }
}