package com.zackmarvel.slox

// program        → statement* EOF ;
// 
// statement      → exprStmt
//                | valStmt
//                | varStmt
//
// exprStmt       → expression ";" ;
// valStmt        → "val" IDENTIFIER "=" expression ";"
// varStmt        → "var" IDENTIFIER "=" expression ";"

trait Stmt

case class Program(body: Seq[Stmt])
    extends Stmt
case class ExprStmt(expr: Expr)
    extends Stmt
case class ValDeclaration(name: Token, expr: Expr)
    extends Stmt
case class VarDeclaration(name: Token, expr: Expr)
  extends Stmt
