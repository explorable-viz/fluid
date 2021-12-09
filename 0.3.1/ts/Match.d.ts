import { Annotation } from "./util/Lattice";
import { List } from "./BaseTypes";
import { DataValue, ExplValue } from "./DataValue";
import { Env } from "./Env";
import { Expr } from "./Expr";
import { Id, Str } from "./Value";
import Cont = Expr.Cont;
declare type MatchPrefix = List<ExplValue<DataValue>>;
export declare class Match<K> extends DataValue<"Match"> {
    tv̅: MatchPrefix;
    κ: K;
}
export declare function match<K extends Cont>(ξ: MatchPrefix, κ: K): Match<K>;
export declare abstract class Elim<K extends Cont = Cont> extends DataValue<"Elim"> {
    apply(tv: ExplValue): [Env, Match<K>];
}
export declare abstract class DataElim<K extends Cont = Cont> extends Elim<K> {
    static is<K extends Cont>(σ: Elim<K>): σ is DataElim<K>;
    static join<K extends Cont>(σ: Elim<K>, τ: Elim<K>): Elim<K>;
}
export declare function dataElim<K extends Cont>(...cκ̅: [string, K][]): (k: Id) => Elim<K>;
export declare class VarElim<K extends Cont> extends Elim<K> {
    x: Str;
    κ: K;
    static is<K extends Cont>(σ: Elim<K>): σ is VarElim<K>;
}
export declare function varElim<K extends Cont>(x: Str, κ: K): (k: Id) => VarElim<K>;
export declare function apply_fwd(ξ: Match<Expr>): Annotation;
export declare function apply_bwd(ξ: Match<Expr>, α: Annotation): void;
export {};
