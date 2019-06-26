// Generated automatically by nearley, version 2.16.0
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d: any[]): any { return d[0]; }

export interface Token { value: any; [key: string]: any };

export interface Lexer {
  reset: (chunk: string, info: any) => void;
  next: () => Token | undefined;
  save: () => any;
  formatError: (token: Token) => string;
  has: (tokenType: string) => boolean
};

export interface NearleyRule {
  name: string;
  symbols: NearleySymbol[];
  postprocess?: (d: any[], loc?: number, reject?: {}) => any
};

export type NearleySymbol = string | { literal: any } | { test: (token: any) => boolean };

export var Lexer: Lexer | undefined = undefined;

export var ParserRules: NearleyRule[] = [
    {"name": "rootExpr", "symbols": ["_", "expr"]},
    {"name": "expr", "symbols": ["compareExpr"]},
    {"name": "compareExpr", "symbols": ["compareExpr", "_", "compareOp", "_", "sumExpr"]},
    {"name": "compareExpr", "symbols": ["sumExpr"]},
    {"name": "sumExpr", "symbols": ["sumExpr", "sumOp", "productExpr"]},
    {"name": "sumExpr", "symbols": ["productExpr"]},
    {"name": "productExpr", "symbols": ["productExpr", "_", "productOp", "_", "exponentExpr"]},
    {"name": "productExpr", "symbols": ["exponentExpr"]},
    {"name": "exponentExpr", "symbols": ["exponentExpr", "_", "exponentOp", "_", "appChain"]},
    {"name": "exponentExpr", "symbols": ["appChain"]},
    {"name": "appChain", "symbols": ["simpleExpr"]},
    {"name": "simpleExpr", "symbols": ["variable"]},
    {"name": "simpleExpr", "symbols": ["number"]},
    {"name": "simpleExpr", "symbols": ["parenthExpr"]},
    {"name": "variable$ebnf$1", "symbols": []},
    {"name": "variable$ebnf$1", "symbols": ["variable$ebnf$1", /[0-9a-zA-Z_]/], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "variable", "symbols": [/[a-zA-Z_]/, "variable$ebnf$1"]},
    {"name": "number$macrocall$2", "symbols": ["number_"]},
    {"name": "number$macrocall$1", "symbols": ["number$macrocall$2", "_"]},
    {"name": "number", "symbols": ["number$macrocall$1"]},
    {"name": "parenthExpr", "symbols": [{"literal":"("}, "_", "expr", "_", {"literal":")"}]},
    {"name": "compareOp$string$1", "symbols": [{"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp", "symbols": ["compareOp$string$1"]},
    {"name": "compareOp$string$2", "symbols": [{"literal":"="}, {"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp", "symbols": ["compareOp$string$2"]},
    {"name": "compareOp$string$3", "symbols": [{"literal":"<"}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp", "symbols": ["compareOp$string$3"]},
    {"name": "compareOp$string$4", "symbols": [{"literal":"<"}, {"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp", "symbols": ["compareOp$string$4"]},
    {"name": "compareOp", "symbols": [{"literal":"<"}]},
    {"name": "compareOp$string$5", "symbols": [{"literal":">"}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp", "symbols": ["compareOp$string$5"]},
    {"name": "compareOp$string$6", "symbols": [{"literal":">"}, {"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp", "symbols": ["compareOp$string$6"]},
    {"name": "compareOp", "symbols": [{"literal":">"}]},
    {"name": "exponentOp$string$1", "symbols": [{"literal":"*"}, {"literal":"*"}], "postprocess": (d) => d.join('')},
    {"name": "exponentOp", "symbols": ["exponentOp$string$1"]},
    {"name": "productOp", "symbols": [{"literal":"*"}]},
    {"name": "productOp", "symbols": [{"literal":"/"}]},
    {"name": "sumOp$macrocall$2", "symbols": [{"literal":"+"}]},
    {"name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2", "_"]},
    {"name": "sumOp", "symbols": ["sumOp$macrocall$1"]},
    {"name": "sumOp$macrocall$4", "symbols": [{"literal":"-"}]},
    {"name": "sumOp$macrocall$3", "symbols": ["sumOp$macrocall$4", "_"]},
    {"name": "sumOp", "symbols": ["sumOp$macrocall$3"]},
    {"name": "sumOp$macrocall$6$string$1", "symbols": [{"literal":"+"}, {"literal":"+"}], "postprocess": (d) => d.join('')},
    {"name": "sumOp$macrocall$6", "symbols": ["sumOp$macrocall$6$string$1"]},
    {"name": "sumOp$macrocall$5", "symbols": ["sumOp$macrocall$6", "_"]},
    {"name": "sumOp", "symbols": ["sumOp$macrocall$5"]},
    {"name": "number_", "symbols": ["int"]},
    {"name": "int", "symbols": [/[0]/]},
    {"name": "int$ebnf$1", "symbols": []},
    {"name": "int$ebnf$1", "symbols": ["int$ebnf$1", "DIGIT"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "int", "symbols": ["digit1to9", "int$ebnf$1"]},
    {"name": "digit1to9", "symbols": [/[1-9]/]},
    {"name": "DIGIT", "symbols": [/[0-9]/]},
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1$subexpression$1", "symbols": ["whitespace"]},
    {"name": "_$ebnf$1$subexpression$1", "symbols": ["singleLineComment"]},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", "_$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "_", "symbols": ["_$ebnf$1"]},
    {"name": "whitespace$ebnf$1", "symbols": [/[\s]/]},
    {"name": "whitespace$ebnf$1", "symbols": ["whitespace$ebnf$1", /[\s]/], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "whitespace", "symbols": ["whitespace$ebnf$1"], "postprocess": d => null},
    {"name": "singleLineComment$string$1", "symbols": [{"literal":"/"}, {"literal":"/"}], "postprocess": (d) => d.join('')},
    {"name": "singleLineComment$ebnf$1", "symbols": []},
    {"name": "singleLineComment$ebnf$1", "symbols": ["singleLineComment$ebnf$1", /[^\n]/], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "singleLineComment", "symbols": ["singleLineComment$string$1", "singleLineComment$ebnf$1"], "postprocess": d => null}
];

export var ParserStart: string = "rootExpr";
