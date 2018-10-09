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
 * Tests of the parser of the Lintilla language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests that check that the parser works correctly.  I.e., it accepts correct
  * input and produces the appropriate trees, and it rejects illegal input.
  */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

  import LintillaTree._

  val parsers = new SyntaxAnalysis(positions)
  import parsers._

  // Tests of parsing terminals

  test("parsing an identifier of one letter produces the correct tree") {
    identifier("x") should parseTo[String]("x")
  }

  test("parsing an identifier as an applied instance produces the correct tree") {
    idnuse("count") should parseTo[IdnUse](IdnUse("count"))
  }

  test("parsing an identifier containing digits and underscores produces the correct tree") {
    idndef("x1_2_3") should parseTo[IdnDef](IdnDef("x1_2_3"))
  }

  test("parsing an integer as an identifier gives an error") {
    identifier("42") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
  }

  test("parsing a non-identifier as an identifier gives an error (digit)") {
    identifier("4foo") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
  }

  test("parsing a non-identifier as an identifier gives an error (underscore)") {
    identifier("_f3") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '_' found")
  }

  test("parsing a keyword as an identifier gives an error") {
    identifier("let ") should failParseAt(1, 1,
                                          "identifier expected but keyword found")
  }

  test("parsing a keyword prefix as an identifier produces the correct tree") {
    identifier("letter") should parseTo[String]("letter")
  }

  test("parsing an integer of one digit as an integer produces the correct tree") {
    integer("8") should parseTo[String]("8")
  }

  test("parsing an integer as an integer produces the correct tree") {
    integer("99") should parseTo[String]("99")
  }

  test("parsing a non-integer as an integer gives an error") {
    integer("total") should
    failParseAt(1, 1,
                "string matching regex '[0-9]+' expected but 't' found")
  }

  // FIXME Add tests of your parsers.
  test("parsing an assign expression produces the correct tree") {
    exp("x := 4") should parseTo[Expression](AssignExp(IdnUse("x"), IntExp(4)))
  }

  test("parsing a return expression produces the correct tree") {
    exp("return 0") should parseTo[Expression](Return(Some(IntExp(0))))
  }

  test("parsing an empty return expression produces the correct tree") {
    exp("return") should parseTo[Expression](Return(None))
  }

  test("parsing an if expression with empty else produces the correct tree") {
    exp("if n < 0 { return 1 } else {}") should parseTo[Expression](IfExp(LessExp(IdnExp(IdnUse("n")), IntExp(0)), Block(Vector(Return(Some(IntExp(1))))), Block(Vector())))
  }

  test("parsing an if expression with else expression produces the correct tree") {
    exp("if n < 0 { return 1 } else { return 2}") should parseTo[Expression](IfExp(LessExp(IdnExp(IdnUse("n")), IntExp(0)), Block(Vector(Return(Some(IntExp(1))))), Block(Vector(Return(Some(IntExp(2)))))))
  }

  test("parsing a while expression produces the correct tree") {
    exp("while n < 0 { n + 1 }") should parseTo[Expression](WhileExp(LessExp(IdnExp(IdnUse("n")), IntExp(0)), PlusExp(IdnExp(IdnUse("n")), IntExp(1))))
  }
}

