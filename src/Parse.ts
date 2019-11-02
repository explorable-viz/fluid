// Generated automatically by nearley, version 2.19.0
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any { return d[0]; }
declare var whitespace: any;
declare var singleLineComment: any;
declare var ident: any;
declare var string: any;
declare var number: any;
declare var compareOp: any;
declare var exponentOp: any;
declare var productOp: any;
declare var sumOp: any;

const moo = require('moo')
const lexer = moo.compile({
   ident: {
      match: /[a-zA-Z_][0-9a-zA-Z_]*'*/, // greedy
      type: moo.keywords({
        keyword: ["_", "as", "match", "fun", "in", "let", "letrec", "primitive", "typematch"],
      })
   },
   whitespace: {
      match: /[ \f\t\r\n]+/,
      lineBreaks: true
   },
   singleLineComment: /\/\/.*$/,
   // JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
   // Seems Moo requires us to use non-capturing groups (?:)
   number: /\-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[e|E][-|+]?[0-9]+)?/,
   string: /"(?:\\["\\]|[^\n"\\])*"/,
   // not quite sure why I can't use literals here:
   sumOp: /\-|\+\+|\+/,
   exponentOp: /\*\*/,
   productOp: /\*|\//, // must come after exponentOp
   compareOp: /===|==|<==|<=|<|>==|>=|>/,
   symbol: ["(", ")", "=", "→", ";", "{", "}", ",", "[", "]", "..."], // must come after compareOp
})


import { __check, assert, error } from "./util/Core"
import { Cons, List, Nil, Pair } from "./BaseTypes"
import { Ctr, ctrFor, exprClass, types } from "./DataType"
import { Expr } from "./Expr"
import { singleton, unionWith } from "./FiniteMap"
import { DataElim, dataElim, varElim } from "./Match"
import { Str } from "./Value"
import { ν, at, num, str } from "./Versioned"

import Cont = Expr.Cont

// Constructors must start with an uppercase letter, a la Haskell. Will fix this as part of issue #49.
function isCtr (str: string): boolean {
   const ch: string = str.charAt(0)
   return ch === ch.toUpperCase() && ch !== ch.toLowerCase()
}

type MkCont = (κ: Cont) => Cont

function compose (mk_κ1: MkCont, mk_κ2: MkCont): MkCont {
   return (κ: Cont) => mk_κ1(mk_κ2(κ))
}

interface NearleyToken {  value: any;
  [key: string]: any;
};

interface NearleyLexer {
  reset: (chunk: string, info: any) => void;
  next: () => NearleyToken | undefined;
  save: () => any;
  formatError: (token: NearleyToken) => string;
  has: (tokenType: string) => boolean;
};

interface NearleyRule {
  name: string;
  symbols: NearleySymbol[];
  postprocess?: (d: any[], loc?: number, reject?: {}) => any;
};

type NearleySymbol = string | { literal: any } | { test: (token: any) => boolean };

interface Grammar {
  Lexer: NearleyLexer | undefined;
  ParserRules: NearleyRule[];
  ParserStart: string;
};

const grammar: Grammar = {
  Lexer: lexer,
  ParserRules: [
    {"name": "rootExpr", "symbols": ["_", "expr"], "postprocess": ([, e]) => e},
    {"name": "rootExpr", "symbols": ["expr"], "postprocess": id},
    {"name": "_$ebnf$1$subexpression$1", "symbols": [(lexer.has("whitespace") ? {type: "whitespace"} : whitespace)]},
    {"name": "_$ebnf$1$subexpression$1", "symbols": [(lexer.has("singleLineComment") ? {type: "singleLineComment"} : singleLineComment)]},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1$subexpression$1"]},
    {"name": "_$ebnf$1$subexpression$2", "symbols": [(lexer.has("whitespace") ? {type: "whitespace"} : whitespace)]},
    {"name": "_$ebnf$1$subexpression$2", "symbols": [(lexer.has("singleLineComment") ? {type: "singleLineComment"} : singleLineComment)]},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", "_$ebnf$1$subexpression$2"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "_", "symbols": ["_$ebnf$1"]},
    {"name": "expr", "symbols": ["compareExpr"], "postprocess": id},
    {"name": "expr", "symbols": ["defs1"], "postprocess": id},
    {"name": "expr", "symbols": ["fun"], "postprocess": id},
    {"name": "expr", "symbols": ["matchAs"], "postprocess": id},
    {"name": "expr", "symbols": ["typematch"], "postprocess": id},
    {"name": "defs1$macrocall$2", "symbols": [{"literal":"in"}]},
    {"name": "defs1$macrocall$1$macrocall$2", "symbols": ["defs1$macrocall$2"]},
    {"name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "defs1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$1"]},
    {"name": "defs1", "symbols": ["defList", "defs1$macrocall$1", "expr"], "postprocess": ([defs, , e]) => Expr.defs(defs, e)(ν())},
    {"name": "compareExpr", "symbols": ["compareExpr", "compareOp", "sumExpr"], "postprocess": ([e1, op, e2]) => Expr.binaryApp(e1, str(op)(ν()), e2)(ν())},
    {"name": "compareExpr", "symbols": ["sumExpr"], "postprocess": id},
    {"name": "sumExpr", "symbols": ["sumExpr", "sumOp", "productExpr"], "postprocess": ([e1, op, e2]) => Expr.binaryApp(e1, str(op)(ν()), e2)(ν())},
    {"name": "sumExpr", "symbols": ["productExpr"], "postprocess": id},
    {"name": "productExpr", "symbols": ["productExpr", "productOp", "exponentExpr"], "postprocess": ([e1, op, e2]) => Expr.binaryApp(e1, str(op)(ν()), e2)(ν())},
    {"name": "productExpr", "symbols": ["exponentExpr"], "postprocess": id},
    {"name": "exponentExpr", "symbols": ["exponentExpr", "exponentOp", "appChain"], "postprocess": ([e1, op, e2]) => Expr.binaryApp(e1, str(op)(ν()), e2)(ν())},
    {"name": "exponentExpr", "symbols": ["appChain"], "postprocess": id},
    {"name": "appChain", "symbols": ["simpleExpr"], "postprocess": id},
    {"name": "appChain", "symbols": ["appChain", "simpleExpr"], "postprocess": ([e1, e2]) => Expr.app(e1, e2)(ν())},
    {"name": "simpleExpr", "symbols": ["variable"], "postprocess": id},
    {"name": "simpleExpr", "symbols": ["string"], "postprocess": id},
    {"name": "simpleExpr", "symbols": ["number"], "postprocess": id},
    {"name": "simpleExpr", "symbols": ["parenthExpr"], "postprocess": id},
    {"name": "simpleExpr", "symbols": ["pair"], "postprocess": id},
    {"name": "simpleExpr", "symbols": ["list"], "postprocess": id},
    {"name": "simpleExpr", "symbols": ["constr"], "postprocess": id},
    {"name": "variable", "symbols": ["var"], "postprocess": ([x]) => Expr.var_(x)(ν())},
    {"name": "var$macrocall$2", "symbols": [(lexer.has("ident") ? {type: "ident"} : ident)]},
    {"name": "var$macrocall$1", "symbols": ["var$macrocall$2"], "postprocess": id},
    {"name": "var$macrocall$1", "symbols": ["var$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "var", "symbols": ["var$macrocall$1"], "postprocess":  ([[x]], _, reject) => {
           if (isCtr(x.value)) {
              return reject
           }
           return str(x.value)(ν()) 
        } },
    {"name": "string$macrocall$2", "symbols": [(lexer.has("string") ? {type: "string"} : string)]},
    {"name": "string$macrocall$1", "symbols": ["string$macrocall$2"], "postprocess": id},
    {"name": "string$macrocall$1", "symbols": ["string$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "string", "symbols": ["string$macrocall$1"], "postprocess": ([[lit]]) => Expr.constStr(str((lit.value as string).slice(1, -1))(ν()))(ν())},
    {"name": "number$macrocall$2", "symbols": [(lexer.has("number") ? {type: "number"} : number)]},
    {"name": "number$macrocall$1", "symbols": ["number$macrocall$2"], "postprocess": id},
    {"name": "number$macrocall$1", "symbols": ["number$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "number", "symbols": ["number$macrocall$1"], "postprocess": ([[lit]]) => Expr.constNum(num(new Number(lit.value as string).valueOf())(ν()))(ν())},
    {"name": "parenthExpr$macrocall$2", "symbols": [{"literal":"("}]},
    {"name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2"], "postprocess": id},
    {"name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "parenthExpr$macrocall$4", "symbols": [{"literal":")"}]},
    {"name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4"], "postprocess": id},
    {"name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "parenthExpr", "symbols": ["parenthExpr$macrocall$1", "expr", "parenthExpr$macrocall$3"], "postprocess": ([, e,]) => e},
    {"name": "pair$macrocall$2", "symbols": [{"literal":"("}]},
    {"name": "pair$macrocall$1", "symbols": ["pair$macrocall$2"], "postprocess": id},
    {"name": "pair$macrocall$1", "symbols": ["pair$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "pair$macrocall$4", "symbols": [{"literal":","}]},
    {"name": "pair$macrocall$3", "symbols": ["pair$macrocall$4"], "postprocess": id},
    {"name": "pair$macrocall$3", "symbols": ["pair$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "pair$macrocall$6", "symbols": [{"literal":")"}]},
    {"name": "pair$macrocall$5", "symbols": ["pair$macrocall$6"], "postprocess": id},
    {"name": "pair$macrocall$5", "symbols": ["pair$macrocall$6", "_"], "postprocess": ([x, ]) => x},
    {"name": "pair", "symbols": ["pair$macrocall$1", "expr", "pair$macrocall$3", "expr", "pair$macrocall$5"], "postprocess": ([, e1, , e2,]) => at(exprClass(Pair), e1, e2)(ν())},
    {"name": "list$macrocall$2", "symbols": [{"literal":"["}]},
    {"name": "list$macrocall$1", "symbols": ["list$macrocall$2"], "postprocess": id},
    {"name": "list$macrocall$1", "symbols": ["list$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "list$macrocall$4", "symbols": [{"literal":"]"}]},
    {"name": "list$macrocall$3", "symbols": ["list$macrocall$4"], "postprocess": id},
    {"name": "list$macrocall$3", "symbols": ["list$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "list", "symbols": ["list$macrocall$1", "listOpt", "list$macrocall$3"], "postprocess": ([, e, ]) => e},
    {"name": "constr", "symbols": ["ctr", "args"], "postprocess":  ([c, e̅], _, reject) => {
           assert(c instanceof Str)
           const ctr: Ctr = ctrFor(c.val)
           if (ctr.arity !== e̅.length) {
              return reject
           }
           return at(exprClass(ctr.C), ...e̅)(ν())
        } },
    {"name": "ctr$macrocall$2", "symbols": [(lexer.has("ident") ? {type: "ident"} : ident)]},
    {"name": "ctr$macrocall$1", "symbols": ["ctr$macrocall$2"], "postprocess": id},
    {"name": "ctr$macrocall$1", "symbols": ["ctr$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "ctr", "symbols": ["ctr$macrocall$1"], "postprocess":  ([[x]], _, reject) => {
           if (!isCtr(x.value)) {
              return reject
           }
           return str(x.value)(ν())
        } },
    {"name": "args", "symbols": [], "postprocess": () => []},
    {"name": "args$macrocall$2", "symbols": [{"literal":"("}]},
    {"name": "args$macrocall$1", "symbols": ["args$macrocall$2"], "postprocess": id},
    {"name": "args$macrocall$1", "symbols": ["args$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "args$ebnf$1", "symbols": []},
    {"name": "args$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":","}]},
    {"name": "args$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "args$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "args$ebnf$1$subexpression$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$1", "expr"], "postprocess": ([, e]) => e},
    {"name": "args$ebnf$1", "symbols": ["args$ebnf$1", "args$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "args$macrocall$4", "symbols": [{"literal":")"}]},
    {"name": "args$macrocall$3", "symbols": ["args$macrocall$4"], "postprocess": id},
    {"name": "args$macrocall$3", "symbols": ["args$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "args", "symbols": ["args$macrocall$1", "expr", "args$ebnf$1", "args$macrocall$3"], "postprocess": ([, e, es,]) => [e, ...es]},
    {"name": "typematch$macrocall$2", "symbols": [{"literal":"typematch"}]},
    {"name": "typematch$macrocall$1$macrocall$2", "symbols": ["typematch$macrocall$2"]},
    {"name": "typematch$macrocall$1$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "typematch$macrocall$1$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "typematch$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$1"]},
    {"name": "typematch$macrocall$4", "symbols": [{"literal":"as"}]},
    {"name": "typematch$macrocall$3$macrocall$2", "symbols": ["typematch$macrocall$4"]},
    {"name": "typematch$macrocall$3$macrocall$1", "symbols": ["typematch$macrocall$3$macrocall$2"], "postprocess": id},
    {"name": "typematch$macrocall$3$macrocall$1", "symbols": ["typematch$macrocall$3$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "typematch$macrocall$3", "symbols": ["typematch$macrocall$3$macrocall$1"]},
    {"name": "typematch", "symbols": ["typematch$macrocall$1", "expr", "typematch$macrocall$3", "typeMatches"], "postprocess": ([, e, , m]) => Expr.typematch(e, m)(ν())},
    {"name": "defList$ebnf$1", "symbols": []},
    {"name": "defList$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":";"}]},
    {"name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "defList$ebnf$1$subexpression$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$1", "def"], "postprocess": ([, def]) => def},
    {"name": "defList$ebnf$1", "symbols": ["defList$ebnf$1", "defList$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "defList", "symbols": ["def", "defList$ebnf$1"], "postprocess": ([def, defs]) => List.fromArray([def, ...defs])},
    {"name": "def", "symbols": ["let"], "postprocess": id},
    {"name": "def", "symbols": ["letrec"], "postprocess": id},
    {"name": "def", "symbols": ["prim"], "postprocess": id},
    {"name": "let$macrocall$2", "symbols": [{"literal":"let"}]},
    {"name": "let$macrocall$1$macrocall$2", "symbols": ["let$macrocall$2"]},
    {"name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "let$macrocall$1", "symbols": ["let$macrocall$1$macrocall$1"]},
    {"name": "let$macrocall$4", "symbols": [{"literal":"="}]},
    {"name": "let$macrocall$3", "symbols": ["let$macrocall$4"], "postprocess": id},
    {"name": "let$macrocall$3", "symbols": ["let$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "let", "symbols": ["let$macrocall$1", "var", "let$macrocall$3", "expr"], "postprocess": ([, x, , e]) => Expr.let_(x, e)(ν())},
    {"name": "letrec$macrocall$2", "symbols": [{"literal":"letrec"}]},
    {"name": "letrec$macrocall$1$macrocall$2", "symbols": ["letrec$macrocall$2"]},
    {"name": "letrec$macrocall$1$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "letrec$macrocall$1$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "letrec$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$1"]},
    {"name": "letrec$ebnf$1", "symbols": []},
    {"name": "letrec$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":";"}]},
    {"name": "letrec$ebnf$1$subexpression$1$macrocall$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "letrec$ebnf$1$subexpression$1$macrocall$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "letrec$ebnf$1$subexpression$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$1", "recDef"], "postprocess": ([, recDef]) => recDef},
    {"name": "letrec$ebnf$1", "symbols": ["letrec$ebnf$1", "letrec$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "letrec", "symbols": ["letrec$macrocall$1", "recDef", "letrec$ebnf$1"], "postprocess": ([, recDef, δ]) => Expr.letRec(List.fromArray([recDef, ...δ]))(ν())},
    {"name": "prim$macrocall$2", "symbols": [{"literal":"primitive"}]},
    {"name": "prim$macrocall$1$macrocall$2", "symbols": ["prim$macrocall$2"]},
    {"name": "prim$macrocall$1$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "prim$macrocall$1$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "prim$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$1"]},
    {"name": "prim", "symbols": ["prim$macrocall$1", "var"], "postprocess": ([, x]) => Expr.prim(x)(ν())},
    {"name": "recDef$macrocall$2", "symbols": [{"literal":"fun"}]},
    {"name": "recDef$macrocall$1$macrocall$2", "symbols": ["recDef$macrocall$2"]},
    {"name": "recDef$macrocall$1$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "recDef$macrocall$1$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "recDef$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$1"]},
    {"name": "recDef", "symbols": ["recDef$macrocall$1", "var", "matches"], "postprocess": ([, f, σ]) => Expr.recDef(f, σ)(ν())},
    {"name": "fun$macrocall$2", "symbols": [{"literal":"fun"}]},
    {"name": "fun$macrocall$1$macrocall$2", "symbols": ["fun$macrocall$2"]},
    {"name": "fun$macrocall$1$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "fun$macrocall$1$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "fun$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$1"]},
    {"name": "fun", "symbols": ["fun$macrocall$1", "matches"], "postprocess": ([, σ]) => Expr.fun(σ)(ν())},
    {"name": "matchAs$macrocall$2", "symbols": [{"literal":"match"}]},
    {"name": "matchAs$macrocall$1$macrocall$2", "symbols": ["matchAs$macrocall$2"]},
    {"name": "matchAs$macrocall$1$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "matchAs$macrocall$1$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "matchAs$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$1"]},
    {"name": "matchAs$macrocall$4", "symbols": [{"literal":"as"}]},
    {"name": "matchAs$macrocall$3$macrocall$2", "symbols": ["matchAs$macrocall$4"]},
    {"name": "matchAs$macrocall$3$macrocall$1", "symbols": ["matchAs$macrocall$3$macrocall$2"], "postprocess": id},
    {"name": "matchAs$macrocall$3$macrocall$1", "symbols": ["matchAs$macrocall$3$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "matchAs$macrocall$3", "symbols": ["matchAs$macrocall$3$macrocall$1"]},
    {"name": "matchAs", "symbols": ["matchAs$macrocall$1", "expr", "matchAs$macrocall$3", "matches"], "postprocess": ([, e, , σ]) => Expr.matchAs(e, σ)(ν())},
    {"name": "matches", "symbols": ["match"], "postprocess": id},
    {"name": "matches$macrocall$2", "symbols": [{"literal":"{"}]},
    {"name": "matches$macrocall$1", "symbols": ["matches$macrocall$2"], "postprocess": id},
    {"name": "matches$macrocall$1", "symbols": ["matches$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "matches$ebnf$1", "symbols": []},
    {"name": "matches$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":";"}]},
    {"name": "matches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "matches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "matches$ebnf$1$subexpression$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$1", "match"], "postprocess": ([, m]) => m},
    {"name": "matches$ebnf$1", "symbols": ["matches$ebnf$1", "matches$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "matches$macrocall$4", "symbols": [{"literal":"}"}]},
    {"name": "matches$macrocall$3", "symbols": ["matches$macrocall$4"], "postprocess": id},
    {"name": "matches$macrocall$3", "symbols": ["matches$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "matches", "symbols": ["matches$macrocall$1", "match", "matches$ebnf$1", "matches$macrocall$3"], "postprocess": ([, m, ms,]) => [m, ...ms].reduce(DataElim.join)},
    {"name": "match$macrocall$2", "symbols": [{"literal":"→"}]},
    {"name": "match$macrocall$1", "symbols": ["match$macrocall$2"], "postprocess": id},
    {"name": "match$macrocall$1", "symbols": ["match$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "match", "symbols": ["pattern", "match$macrocall$1", "expr"], "postprocess": ([mk_κ, , e]) => mk_κ(e)},
    {"name": "match", "symbols": ["pattern", "matches"], "postprocess": ([mk_κ1, σ]) => mk_κ1(Expr.fun(σ)(ν()))},
    {"name": "typeMatches", "symbols": ["typeMatch"], "postprocess": id},
    {"name": "typeMatches$macrocall$2", "symbols": [{"literal":"{"}]},
    {"name": "typeMatches$macrocall$1", "symbols": ["typeMatches$macrocall$2"], "postprocess": id},
    {"name": "typeMatches$macrocall$1", "symbols": ["typeMatches$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "typeMatches$ebnf$1", "symbols": []},
    {"name": "typeMatches$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":";"}]},
    {"name": "typeMatches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "typeMatches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "typeMatches$ebnf$1$subexpression$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$1", "typeMatch"], "postprocess": ([, m]) => m},
    {"name": "typeMatches$ebnf$1", "symbols": ["typeMatches$ebnf$1", "typeMatches$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "typeMatches$macrocall$4", "symbols": [{"literal":"}"}]},
    {"name": "typeMatches$macrocall$3", "symbols": ["typeMatches$macrocall$4"], "postprocess": id},
    {"name": "typeMatches$macrocall$3", "symbols": ["typeMatches$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "typeMatches", "symbols": ["typeMatches$macrocall$1", "typeMatch", "typeMatches$ebnf$1", "typeMatches$macrocall$3"], "postprocess": ([, m, ms,]) => [m, ...ms].reduce((m1, m2) => unionWith(m1, m2, (e: Expr, eʹ: Expr): Expr => error("Overlapping typecase branches.")))},
    {"name": "typeMatch$macrocall$2", "symbols": [{"literal":"→"}]},
    {"name": "typeMatch$macrocall$1", "symbols": ["typeMatch$macrocall$2"], "postprocess": id},
    {"name": "typeMatch$macrocall$1", "symbols": ["typeMatch$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "typeMatch", "symbols": ["typename", "typeMatch$macrocall$1", "expr"], "postprocess":  ([x, , e]) => {
           assert(x instanceof Str)
           if (!types.has(x.val)) {
              error(`Type name ${x.val} not found.`)
           }
           return singleton(x, e)
        } },
    {"name": "typename$macrocall$2", "symbols": [(lexer.has("ident") ? {type: "ident"} : ident)]},
    {"name": "typename$macrocall$1", "symbols": ["typename$macrocall$2"], "postprocess": id},
    {"name": "typename$macrocall$1", "symbols": ["typename$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "typename", "symbols": ["typename$macrocall$1"], "postprocess": ([[x]]) => str(x.value)(ν())},
    {"name": "listOpt", "symbols": [], "postprocess": () => at(exprClass(Nil))(ν())},
    {"name": "listOpt$ebnf$1", "symbols": []},
    {"name": "listOpt$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":","}]},
    {"name": "listOpt$ebnf$1$subexpression$1$macrocall$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "listOpt$ebnf$1$subexpression$1$macrocall$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "listOpt$ebnf$1$subexpression$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$1", "expr"], "postprocess": ([, e]) => e},
    {"name": "listOpt$ebnf$1", "symbols": ["listOpt$ebnf$1", "listOpt$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "listOpt", "symbols": ["expr", "listOpt$ebnf$1", "listRestOpt"], "postprocess": ([e, es, eʹ]) => [e, ...es, eʹ].reverse().reduce((e̅, e) => at(exprClass(Cons), e, e̅)(ν()))},
    {"name": "listRestOpt", "symbols": [], "postprocess": () => at(exprClass(Nil))(ν())},
    {"name": "listRestOpt$macrocall$2", "symbols": [{"literal":","}]},
    {"name": "listRestOpt$macrocall$1", "symbols": ["listRestOpt$macrocall$2"], "postprocess": id},
    {"name": "listRestOpt$macrocall$1", "symbols": ["listRestOpt$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "listRestOpt$macrocall$4", "symbols": [{"literal":"..."}]},
    {"name": "listRestOpt$macrocall$3", "symbols": ["listRestOpt$macrocall$4"], "postprocess": id},
    {"name": "listRestOpt$macrocall$3", "symbols": ["listRestOpt$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "listRestOpt", "symbols": ["listRestOpt$macrocall$1", "listRestOpt$macrocall$3", "expr"], "postprocess": ([, , e]) => e},
    {"name": "pattern", "symbols": ["variable_pattern"], "postprocess": id},
    {"name": "pattern", "symbols": ["pair_pattern"], "postprocess": id},
    {"name": "pattern", "symbols": ["list_pattern"], "postprocess": id},
    {"name": "pattern", "symbols": ["constr_pattern"], "postprocess": id},
    {"name": "variable_pattern$macrocall$2", "symbols": [{"literal":"_"}]},
    {"name": "variable_pattern$macrocall$1$macrocall$2", "symbols": ["variable_pattern$macrocall$2"]},
    {"name": "variable_pattern$macrocall$1$macrocall$1", "symbols": ["variable_pattern$macrocall$1$macrocall$2"], "postprocess": id},
    {"name": "variable_pattern$macrocall$1$macrocall$1", "symbols": ["variable_pattern$macrocall$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "variable_pattern$macrocall$1", "symbols": ["variable_pattern$macrocall$1$macrocall$1"]},
    {"name": "variable_pattern", "symbols": ["variable_pattern$macrocall$1"], "postprocess": () => (κ: Cont) => varElim(str("_")(ν()), κ)(ν())},
    {"name": "variable_pattern", "symbols": ["var"], "postprocess": ([x]) => (κ: Cont) => varElim(x, κ)(ν())},
    {"name": "pair_pattern$macrocall$2", "symbols": [{"literal":"("}]},
    {"name": "pair_pattern$macrocall$1", "symbols": ["pair_pattern$macrocall$2"], "postprocess": id},
    {"name": "pair_pattern$macrocall$1", "symbols": ["pair_pattern$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "pair_pattern$macrocall$4", "symbols": [{"literal":","}]},
    {"name": "pair_pattern$macrocall$3", "symbols": ["pair_pattern$macrocall$4"], "postprocess": id},
    {"name": "pair_pattern$macrocall$3", "symbols": ["pair_pattern$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "pair_pattern$macrocall$6", "symbols": [{"literal":")"}]},
    {"name": "pair_pattern$macrocall$5", "symbols": ["pair_pattern$macrocall$6"], "postprocess": id},
    {"name": "pair_pattern$macrocall$5", "symbols": ["pair_pattern$macrocall$6", "_"], "postprocess": ([x, ]) => x},
    {"name": "pair_pattern", "symbols": ["pair_pattern$macrocall$1", "pattern", "pair_pattern$macrocall$3", "pattern", "pair_pattern$macrocall$5"], "postprocess": ([, mk_κ1, , mk_κ2, ,]) => (κ: Cont) => dataElim([Pair.name, compose(mk_κ1, mk_κ2)(κ)])(ν())},
    {"name": "list_pattern$macrocall$2", "symbols": [{"literal":"["}]},
    {"name": "list_pattern$macrocall$1", "symbols": ["list_pattern$macrocall$2"], "postprocess": id},
    {"name": "list_pattern$macrocall$1", "symbols": ["list_pattern$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "list_pattern$macrocall$4", "symbols": [{"literal":"]"}]},
    {"name": "list_pattern$macrocall$3", "symbols": ["list_pattern$macrocall$4"], "postprocess": id},
    {"name": "list_pattern$macrocall$3", "symbols": ["list_pattern$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "list_pattern", "symbols": ["list_pattern$macrocall$1", "listOpt_pattern", "list_pattern$macrocall$3"], "postprocess": ([, mk_κ, ]) => mk_κ},
    {"name": "listOpt_pattern", "symbols": [], "postprocess": () => (κ: Cont) => dataElim([Nil.name, κ])(ν())},
    {"name": "listOpt_pattern", "symbols": ["list1_pattern"], "postprocess": id},
    {"name": "list1_pattern", "symbols": ["pattern", "listRestOpt_pattern"], "postprocess": ([mk_κ1, mk_κ2]) => (κ: Cont) => dataElim([Cons.name, compose(mk_κ1, mk_κ2)(κ)])(ν())},
    {"name": "listRestOpt_pattern", "symbols": [], "postprocess": () => (κ: Cont) => dataElim([Nil.name, κ])(ν())},
    {"name": "listRestOpt_pattern$macrocall$2", "symbols": [{"literal":","}]},
    {"name": "listRestOpt_pattern$macrocall$1", "symbols": ["listRestOpt_pattern$macrocall$2"], "postprocess": id},
    {"name": "listRestOpt_pattern$macrocall$1", "symbols": ["listRestOpt_pattern$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "listRestOpt_pattern$macrocall$4", "symbols": [{"literal":"..."}]},
    {"name": "listRestOpt_pattern$macrocall$3", "symbols": ["listRestOpt_pattern$macrocall$4"], "postprocess": id},
    {"name": "listRestOpt_pattern$macrocall$3", "symbols": ["listRestOpt_pattern$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "listRestOpt_pattern", "symbols": ["listRestOpt_pattern$macrocall$1", "listRestOpt_pattern$macrocall$3", "pattern"], "postprocess": ([, , mk_κ]) => mk_κ},
    {"name": "listRestOpt_pattern$macrocall$6", "symbols": [{"literal":","}]},
    {"name": "listRestOpt_pattern$macrocall$5", "symbols": ["listRestOpt_pattern$macrocall$6"], "postprocess": id},
    {"name": "listRestOpt_pattern$macrocall$5", "symbols": ["listRestOpt_pattern$macrocall$6", "_"], "postprocess": ([x, ]) => x},
    {"name": "listRestOpt_pattern", "symbols": ["listRestOpt_pattern$macrocall$5", "list1_pattern"], "postprocess": ([, mk_κ]) => mk_κ},
    {"name": "constr_pattern", "symbols": ["ctr", "args_pattern"], "postprocess":  ([c, mk_κs], _, reject) => {
           assert(c instanceof Str)
           if (ctrFor(c.val).arity !== mk_κs.length) {
              return reject
           }
           return (κ: Cont) => dataElim([c.val, mk_κs.reduce(compose, (κ: Cont) => κ)(κ)])(ν())
        } },
    {"name": "args_pattern", "symbols": [], "postprocess": () => []},
    {"name": "args_pattern$macrocall$2", "symbols": [{"literal":"("}]},
    {"name": "args_pattern$macrocall$1", "symbols": ["args_pattern$macrocall$2"], "postprocess": id},
    {"name": "args_pattern$macrocall$1", "symbols": ["args_pattern$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "args_pattern$ebnf$1", "symbols": []},
    {"name": "args_pattern$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":","}]},
    {"name": "args_pattern$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id},
    {"name": "args_pattern$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "args_pattern$ebnf$1$subexpression$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$1", "pattern"], "postprocess": ([, mk_κ]) => mk_κ},
    {"name": "args_pattern$ebnf$1", "symbols": ["args_pattern$ebnf$1", "args_pattern$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "args_pattern$macrocall$4", "symbols": [{"literal":")"}]},
    {"name": "args_pattern$macrocall$3", "symbols": ["args_pattern$macrocall$4"], "postprocess": id},
    {"name": "args_pattern$macrocall$3", "symbols": ["args_pattern$macrocall$4", "_"], "postprocess": ([x, ]) => x},
    {"name": "args_pattern", "symbols": ["args_pattern$macrocall$1", "pattern", "args_pattern$ebnf$1", "args_pattern$macrocall$3"], "postprocess": ([, mk_κ, mk_κs,]) => [mk_κ, ...mk_κs]},
    {"name": "compareOp$macrocall$2", "symbols": [(lexer.has("compareOp") ? {type: "compareOp"} : compareOp)]},
    {"name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2"], "postprocess": id},
    {"name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$1"], "postprocess": ([[x]]) => x.value},
    {"name": "exponentOp$macrocall$2", "symbols": [(lexer.has("exponentOp") ? {type: "exponentOp"} : exponentOp)]},
    {"name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2"], "postprocess": id},
    {"name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "exponentOp", "symbols": ["exponentOp$macrocall$1"], "postprocess": ([[x]]) => x.value},
    {"name": "productOp$macrocall$2", "symbols": [(lexer.has("productOp") ? {type: "productOp"} : productOp)]},
    {"name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2"], "postprocess": id},
    {"name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "productOp", "symbols": ["productOp$macrocall$1"], "postprocess": ([[x]]) => x.value},
    {"name": "sumOp$macrocall$2", "symbols": [(lexer.has("sumOp") ? {type: "sumOp"} : sumOp)]},
    {"name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2"], "postprocess": id},
    {"name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2", "_"], "postprocess": ([x, ]) => x},
    {"name": "sumOp", "symbols": ["sumOp$macrocall$1"], "postprocess": ([[x]]) => x.value}
  ],
  ParserStart: "rootExpr",
};

export default grammar;
