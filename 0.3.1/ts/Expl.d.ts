import { List } from "./BaseTypes";
import { DataValue, ExplValue } from "./DataValue";
import { Eval } from "./Eval";
import { Expr } from "./Expr";
import { Elim, Match } from "./Match";
import { UnaryOp } from "./Primitive";
import { Id, PrimValue, Str } from "./Value";
export declare type Closure = Eval.Closure;
export declare type Expl = Expl.Expl;
export declare namespace Expl {
    abstract class Expl extends DataValue<"Expl"> {
    }
    abstract class NonTerminal extends Expl {
        abstract t: Expl;
    }
    abstract class Terminal extends Expl {
    }
    class App extends NonTerminal {
        tf: ExplValue<Closure>;
        tu: ExplValue;
        δ: List<RecDef>;
        ξ: Match<Expr>;
        t: Expl;
    }
    function app(tf: ExplValue<Closure>, tu: ExplValue, δ: List<RecDef>, ξ: Match<Expr>, t: Expl): (k: Id) => App;
    class UnaryApp extends Terminal {
        tf: ExplValue<UnaryOp>;
        tv: ExplValue<PrimValue>;
    }
    function unaryApp(tf: ExplValue<UnaryOp>, tv: ExplValue<PrimValue>): (k: Id) => UnaryApp;
    class BinaryApp extends Terminal {
        tv1: ExplValue<PrimValue>;
        opName: Str;
        tv2: ExplValue<PrimValue>;
    }
    function binaryApp(tv1: ExplValue<PrimValue>, opName: Str, tv2: ExplValue<PrimValue>): (k: Id) => BinaryApp;
    class DataExpl extends Terminal {
        readonly ctr: string;
        readonly __children: Expl[];
    }
    abstract class Def extends DataValue<"Expl.Def"> {
    }
    class Let extends Def {
        x: Str;
        tv: ExplValue;
    }
    function let_(x: Str, tv: ExplValue): (k: Id) => Let;
    class Prim extends Def {
        x: Str;
        t_op: ExplValue<UnaryOp>;
    }
    function prim(x: Str, t_op: ExplValue<UnaryOp>): (k: Id) => Prim;
    class RecDef extends DataValue<"Expl.RecDef"> {
        x: Str;
        tf: ExplValue<Closure>;
    }
    function recDef(x: Str, tf: ExplValue<Closure>): (k: Id) => RecDef;
    class LetRec extends Def {
        δ: List<RecDef>;
    }
    function letRec(δ: List<RecDef>): (k: Id) => LetRec;
    class Defs extends NonTerminal {
        def̅: List<Def>;
        t: Expl;
    }
    function defs(def̅: List<Def>, t: Expl): (k: Id) => Defs;
    class Const extends Terminal {
    }
    function const_(): (k: Id) => Const;
    class Fun extends Terminal {
        σ: Elim<Expr>;
    }
    function fun(σ: Elim<Expr>): (k: Id) => Fun;
    class MatchAs extends NonTerminal {
        tu: ExplValue;
        ξ: Match<Expr>;
        t: Expl;
    }
    function matchAs(tu: ExplValue, ξ: Match<Expr>, t: Expl): (k: Id) => MatchAs;
    class Quote extends Terminal {
    }
    function quote(): (k: Id) => Quote;
    class Typematch extends NonTerminal {
        tu: ExplValue;
        d: Str;
        t: Expl;
    }
    function typematch(tu: ExplValue, d: Str, t: Expl): (k: Id) => Typematch;
    class Var extends NonTerminal {
        x: Str;
        t: Expl;
    }
    function var_(x: Str, t: Expl): (k: Id) => Var;
    function explChild<T extends DataValue>(t: Expl, v: T, prop: keyof T): ExplValue;
    function explChildren(t: Expl, v: DataValue): ExplValue[];
}
