import { List } from "./BaseTypes";
import { DataValue, ExplValue } from "./DataValue";
import { Env } from "./Env";
import { Expl } from "./Expl";
import { Expr } from "./Expr";
import { Elim } from "./Match";
import { TaggedId } from "./Value";
declare type Def = Expr.Def;
declare type RecDef = Expr.RecDef;
export declare type ExplId = TaggedId<"t">;
export declare type ValId = TaggedId<"v">;
export declare module Eval {
    class Closure extends DataValue<"Closure"> {
        ρ: Env;
        δ: List<RecDef>;
        f: Elim<Expr>;
    }
    function defs(ρ: Env, def̅: List<Def>, ρ_ext: Env): [List<Expl.Def>, Env];
    function eval_(ρ: Env, e: Expr): ExplValue;
    function eval_fwd(e: Expr, tv: ExplValue): void;
    function eval_bwd(e: Expr, tv: ExplValue): void;
}
export {};
