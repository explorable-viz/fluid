import { List } from "./BaseTypes";
import { DataValue } from "./DataValue";
import { FiniteMap } from "./FiniteMap";
import { Elim } from "./Match";
import { Id, Num, Str } from "./Value";
export declare const strings: {
    arrow: string;
    as: string;
    bracketL: string;
    bracketR: string;
    comma: string;
    curlyL: string;
    curlyR: string;
    ellipsis: string;
    equals: string;
    fun: string;
    in_: string;
    let_: string;
    letRec: string;
    match: string;
    primitive: string;
    parenL: string;
    parenR: string;
    quotes: string;
    semicolon: string;
    typematch: string;
};
export declare type Expr = Expr.Expr;
export declare type Cont = Expr.Cont;
export declare namespace Expr {
    export type Cont = Expr | DataValue<"Elim">;
    type SyntaxNodeTag = "Expr" | "Expr.Def" | "Expr.RecDef";
    export abstract class SyntaxNode<Tag extends SyntaxNodeTag = SyntaxNodeTag> extends DataValue<Tag> {
    }
    export abstract class Expr extends SyntaxNode<"Expr"> {
    }
    export class App extends Expr {
        f: Expr;
        e: Expr;
    }
    export function app(f: Expr, e: Expr): (k: Id) => App;
    export class BinaryApp extends Expr {
        e1: Expr;
        opName: Str;
        e2: Expr;
    }
    export function binaryApp(e1: Expr, opName: Str, e2: Expr): (k: Id) => BinaryApp;
    export class ConstNum extends Expr {
        val: Num;
    }
    export function constNum(val: Num): (k: Id) => ConstNum;
    export class ConstStr extends Expr {
        val: Str;
    }
    export function constStr(val: Str): (k: Id) => ConstStr;
    export class DataExpr extends Expr {
        readonly ctr: string;
        __child(prop: keyof this): Expr;
        readonly __children: Expr[];
    }
    export class Def extends SyntaxNode<"Expr.Def"> {
    }
    export class Let extends Def {
        x: Str;
        e: Expr;
    }
    export function let_(x: Str, e: Expr): (k: Id) => Let;
    export class Prim extends Def {
        x: Str;
    }
    export function prim(x: Str): (k: Id) => Prim;
    export class RecDef extends SyntaxNode<"Expr.RecDef"> {
        x: Str;
        σ: Elim<Expr>;
    }
    export function recDef(x: Str, σ: Elim<Expr>): (k: Id) => RecDef;
    export class LetRec extends Def {
        δ: List<RecDef>;
    }
    export function letRec(δ: List<RecDef>): (k: Id) => LetRec;
    export class Defs extends Expr {
        def̅: List<Def>;
        e: Expr;
    }
    export function defs(def̅: List<Def>, e: Expr): (k: Id) => Defs;
    export class Fun extends Expr {
        σ: Elim<Expr>;
    }
    export function fun(σ: Elim<Expr>): (k: Id) => Fun;
    export class MatchAs extends Expr {
        e: Expr;
        σ: Elim<Expr>;
    }
    export function matchAs(e: Expr, σ: Elim<Expr>): (k: Id) => MatchAs;
    export class Quote extends Expr {
        e: Expr;
    }
    export function quote(e: Expr): (k: Id) => Quote;
    export class Typematch extends Expr {
        e: Expr;
        cases: FiniteMap<Expr>;
    }
    export function typematch(e: Expr, cases: FiniteMap<Expr>): (k: Id) => Typematch;
    export class Var extends Expr {
        x: Str;
    }
    export function var_(x: Str): (k: Id) => Var;
    export function freeVars(e: Expr): Set<string>;
    export {};
}
