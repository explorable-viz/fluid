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
    {"name": "compareExpr", "symbols": ["compareExpr", "compareOp", "sumExpr"]},
    {"name": "compareExpr", "symbols": ["sumExpr"]},
    {"name": "sumExpr", "symbols": ["sumExpr", "sumOp", "productExpr"]},
    {"name": "sumExpr", "symbols": ["productExpr"]},
    {"name": "productExpr", "symbols": ["productExpr", "productOp", "exponentExpr"]},
    {"name": "productExpr", "symbols": ["exponentExpr"]},
    {"name": "exponentExpr", "symbols": ["exponentExpr", "exponentOp", "appChain"]},
    {"name": "exponentExpr", "symbols": ["appChain"]},
    {"name": "appChain", "symbols": ["simpleExpr"]},
    {"name": "simpleExpr", "symbols": ["var"]},
    {"name": "simpleExpr", "symbols": ["number"]},
    {"name": "simpleExpr", "symbols": ["parenthExpr"]},
    {"name": "simpleExpr", "symbols": ["defs1"]},
    {"name": "var$macrocall$2$ebnf$1", "symbols": []},
    {"name": "var$macrocall$2$ebnf$1", "symbols": ["var$macrocall$2$ebnf$1", /[0-9a-zA-Z_]/], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "var$macrocall$2", "symbols": [/[a-zA-Z_]/, "var$macrocall$2$ebnf$1"]},
    {"name": "var$macrocall$1", "symbols": ["var$macrocall$2", "_"]},
    {"name": "var", "symbols": ["var$macrocall$1"]},
    {"name": "number$macrocall$2", "symbols": ["number_"]},
    {"name": "number$macrocall$1", "symbols": ["number$macrocall$2", "_"]},
    {"name": "number", "symbols": ["number$macrocall$1"]},
    {"name": "parenthExpr$macrocall$2", "symbols": [{"literal":"("}]},
    {"name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2", "_"]},
    {"name": "parenthExpr$macrocall$4", "symbols": [{"literal":")"}]},
    {"name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4", "_"]},
    {"name": "parenthExpr", "symbols": ["parenthExpr$macrocall$1", "expr", "parenthExpr$macrocall$3"]},
    {"name": "defs1$macrocall$2$string$1", "symbols": [{"literal":"i"}, {"literal":"n"}], "postprocess": (d) => d.join('')},
    {"name": "defs1$macrocall$2", "symbols": ["defs1$macrocall$2$string$1"]},
    {"name": "defs1$macrocall$1$macrocall$2", "symbols": ["defs1$macrocall$2"]},
    {"name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2", "_"]},
    {"name": "defs1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$1"]},
    {"name": "defs1", "symbols": ["defList", "defs1$macrocall$1", "expr"]},
    {"name": "defList$ebnf$1", "symbols": []},
    {"name": "defList$ebnf$1$subexpression$1$macrocall$2", "symbols": [{"literal":";"}]},
    {"name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2", "_"]},
    {"name": "defList$ebnf$1$subexpression$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$1", "def"]},
    {"name": "defList$ebnf$1", "symbols": ["defList$ebnf$1", "defList$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]])},
    {"name": "defList", "symbols": ["def", "defList$ebnf$1"]},
    {"name": "def", "symbols": ["let"]},
    {"name": "let$macrocall$2$string$1", "symbols": [{"literal":"l"}, {"literal":"e"}, {"literal":"t"}], "postprocess": (d) => d.join('')},
    {"name": "let$macrocall$2", "symbols": ["let$macrocall$2$string$1"]},
    {"name": "let$macrocall$1$macrocall$2", "symbols": ["let$macrocall$2"]},
    {"name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2", "_"]},
    {"name": "let$macrocall$1", "symbols": ["let$macrocall$1$macrocall$1"]},
    {"name": "let$macrocall$4", "symbols": [{"literal":"="}]},
    {"name": "let$macrocall$3", "symbols": ["let$macrocall$4", "_"]},
    {"name": "let", "symbols": ["let$macrocall$1", "var", "let$macrocall$3", "expr"]},
    {"name": "compareOp$macrocall$2$string$1", "symbols": [{"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp$macrocall$2", "symbols": ["compareOp$macrocall$2$string$1"]},
    {"name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$1"]},
    {"name": "compareOp$macrocall$4$string$1", "symbols": [{"literal":"="}, {"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp$macrocall$4", "symbols": ["compareOp$macrocall$4$string$1"]},
    {"name": "compareOp$macrocall$3", "symbols": ["compareOp$macrocall$4", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$3"]},
    {"name": "compareOp$macrocall$6$string$1", "symbols": [{"literal":"<"}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp$macrocall$6", "symbols": ["compareOp$macrocall$6$string$1"]},
    {"name": "compareOp$macrocall$5", "symbols": ["compareOp$macrocall$6", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$5"]},
    {"name": "compareOp$macrocall$8$string$1", "symbols": [{"literal":"<"}, {"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp$macrocall$8", "symbols": ["compareOp$macrocall$8$string$1"]},
    {"name": "compareOp$macrocall$7", "symbols": ["compareOp$macrocall$8", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$7"]},
    {"name": "compareOp$macrocall$10", "symbols": [{"literal":"<"}]},
    {"name": "compareOp$macrocall$9", "symbols": ["compareOp$macrocall$10", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$9"]},
    {"name": "compareOp$macrocall$12$string$1", "symbols": [{"literal":">"}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp$macrocall$12", "symbols": ["compareOp$macrocall$12$string$1"]},
    {"name": "compareOp$macrocall$11", "symbols": ["compareOp$macrocall$12", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$11"]},
    {"name": "compareOp$macrocall$14$string$1", "symbols": [{"literal":">"}, {"literal":"="}, {"literal":"="}], "postprocess": (d) => d.join('')},
    {"name": "compareOp$macrocall$14", "symbols": ["compareOp$macrocall$14$string$1"]},
    {"name": "compareOp$macrocall$13", "symbols": ["compareOp$macrocall$14", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$13"]},
    {"name": "compareOp$macrocall$16", "symbols": [{"literal":">"}]},
    {"name": "compareOp$macrocall$15", "symbols": ["compareOp$macrocall$16", "_"]},
    {"name": "compareOp", "symbols": ["compareOp$macrocall$15"]},
    {"name": "exponentOp$macrocall$2$string$1", "symbols": [{"literal":"*"}, {"literal":"*"}], "postprocess": (d) => d.join('')},
    {"name": "exponentOp$macrocall$2", "symbols": ["exponentOp$macrocall$2$string$1"]},
    {"name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2", "_"]},
    {"name": "exponentOp", "symbols": ["exponentOp$macrocall$1"]},
    {"name": "productOp$macrocall$2", "symbols": [{"literal":"*"}]},
    {"name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2", "_"]},
    {"name": "productOp", "symbols": ["productOp$macrocall$1"]},
    {"name": "productOp$macrocall$4", "symbols": [{"literal":"/"}]},
    {"name": "productOp$macrocall$3", "symbols": ["productOp$macrocall$4", "_"]},
    {"name": "productOp", "symbols": ["productOp$macrocall$3"]},
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
