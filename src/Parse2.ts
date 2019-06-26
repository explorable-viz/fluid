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
    {"name": "expr", "symbols": ["compareExpr"]},
    {"name": "compareExpr", "symbols": ["compareExpr", "compareOp", "sumExpr"]},
    {"name": "compareExpr", "symbols": ["sumExpr"]},
    {"name": "sumExpr", "symbols": ["sumExpr", "sumOp", "productExpr"]},
    {"name": "sumExpr", "symbols": ["productExpr"]},
    {"name": "productExpr", "symbols": ["productExpr", "productOp", "exponentExpr"]},
    {"name": "productExpr", "symbols": ["exponentExpr"]},
    {"name": "exponentExpr", "symbols": ["exponentExpr", "exponentOp", "appChain"]},
    {"name": "exponentExpr", "symbols": ["appChain"]},
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
    {"name": "sumOp", "symbols": [{"literal":"+"}]},
    {"name": "sumOp", "symbols": [{"literal":"-"}]},
    {"name": "sumOp$string$1", "symbols": [{"literal":"+"}, {"literal":"+"}], "postprocess": (d) => d.join('')},
    {"name": "sumOp", "symbols": ["sumOp$string$1"]}
];

export var ParserStart: string = "expr";
