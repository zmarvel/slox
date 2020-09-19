package com.zackmarvel.slox;

// program   → statement* EOF ;
// 
// statement → exprStmt
//           | printStmt ;
// 
// exprStmt  → expression ";" ;
// printStmt → "print" expression ";" ;

trait Stmt

case class Program(body: List[Stmt])
    extends Stmt
case class ExprStmt(expr: Expr)
    extends Stmt
case class Print(expr: Expr)
    extends Stmt
