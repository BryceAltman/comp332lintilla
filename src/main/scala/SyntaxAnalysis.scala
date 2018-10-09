/*
 * This file is part of COMP332 Assignment 2 2018.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2018, Dominic Verity and Anthony Sloane, Macquarie University.
 *         All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Parser for the Lintilla language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for Lintilla.
 */
class SyntaxAnalysis (positions : Positions)
    extends Parsers (positions) {

  import LintillaTree._
  import scala.language.postfixOps

  // Top level parser.
  lazy val parser : PackratParser[Program] =
    phrase(program)

  // Parses a whole Lintilla program.
  lazy val program : PackratParser[Program] =
    rep1sep(exp, ";") ^^ Program    // FIXME replace this implementation.

  // FIXME Add your parsers here!
  lazy val exp : PackratParser[Expression] =
      (idnuse <~ ":=") ~ (exp) ^^ AssignExp |
      ("if" ~> exp) ~ (exp) ~ ("else" ~> exp) ^^ IfExp |
      ("while" ~> exp) ~ (exp) ^^ WhileExp |
      ("return") ~ (exp?) ^^ { case r ~ e => Return (e) } |
      ("fn" ~> idndef <~ "(") ~ (repsep(paramdecl, ",") <~ ")") ~ (("->" ~> tipe)?) ~ (exp) ^^ FnDecl |
      ("let" ~> idndef) ~ ("=" ~> exp) ^^ LetDecl |
      ("let" ~ "mut" ~> idndef) ~ ("=" ~> exp) ^^ LetMutDecl |
      pexp
      
  //The following pexp Parsers are ordered for precedence    
  lazy val pexp : PackratParser[Expression] = 
    (pexp <~ "=") ~ (pexp2) ^^ EqualExp |
    (pexp <~ "<") ~ (pexp2) ^^ LessExp |
    pexp2

  lazy val pexp2 : PackratParser[Expression] =  
    (pexp2 <~ "+") ~ (pexp3) ^^ PlusExp |
    (pexp2 <~ "-") ~ (pexp3) ^^ MinusExp |
    pexp3

  lazy val pexp3 : PackratParser[Expression] =
    (pexp3 <~ "*") ~ (pexp4) ^^ StarExp |
    (pexp3 <~ "/") ~ (pexp4) ^^ SlashExp |
    pexp4

  lazy val pexp4 : PackratParser[Expression] =
    ("-" ~> pexp4) ^^ NegExp |
    "true" ^^^ BoolExp(true) |
    "false" ^^^ BoolExp(false) |
    integer ^^ (i => IntExp(i.toInt)) |
    "{" ~> repsep(exp, ";") <~ "}"  ^^ Block |
    "(" ~> (exp) <~ ")"|
    app 
    
  lazy val app : PackratParser[Expression] = 
    (app) ~ (("(" ~> rep1sep(exp, ",")) <~ ")") ^^ AppExp |
    idnuse ^^ IdnExp


  lazy val paramdecl : PackratParser[ParamDecl] = 
      (idndef <~ ":") ~ tipe ^^ ParamDecl

  lazy val tipe : PackratParser[Type] =
    "unit" ^^^ UnitType() |
    "int" ^^^ IntType() |
    "bool" ^^^ BoolType() |
    (("fn" ~ "(") ~> repsep(tipe, ",") <~ ")") ~ (("->" ~> tipe)?) ^^ { 
      case a ~ None => FnType(a, UnitType())
      case a ~ Some(t) => FnType(a, t) } |
    "(" ~> tipe <~ ")"

  // Parses a literal integer.
  lazy val integer : PackratParser[String] =
    regex("[0-9]+".r)

  // Parses a defining occurence of an identifier.
  lazy val idndef : PackratParser[IdnDef] =
    identifier ^^ IdnDef

  // Parses an applied opccurence of an identifier.
  lazy val idnuse : PackratParser[IdnUse] =
    identifier ^^ IdnUse

  // Parses a legal identifier. Checks to ensure that the word parsed is
  // not a Lintilla keyword.
  lazy val identifier : PackratParser[String] =
    (not(keyword) | failure("identifier expected but keyword found")) ~>
      "[a-zA-Z][a-zA-Z0-9_]*".r

  // Parses any legal Lintilla keyword. This parser ensures that the keyword found
  // is not a prefix of an longer identifier. So this parser will not parse the
  // "int" prefix of "integer" as a keyword.
  lazy val keyword =
    keywords("[^a-zA-Z0-9_]".r,
             List("bool", "else", "false", "fn", "if", "int", "let", "mut",
                  "return", "true", "unit", "while")) |
      failure("expecting keyword")

  // We use the character class `\R` here to match line endings, so that we correctly
  // handle all of the end-line variants in un*x, MacOSX, MS Windows, and unicode.
  override val whitespace: Parser[String] =
    """(\s|(//.*(\R|\z)))*""".r
}
