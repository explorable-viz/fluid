import { Annotation } from "./util/Lattice";
import { ExplValue } from "./DataValue";
import { Expr } from "./Expr";
import { Value } from "./Value";
export declare type Annotated = Expr.SyntaxNode | ExplValue;
export declare type Slice = Set<Annotated>;
export declare function annotated(v: Value): v is Annotated;
export declare function isα(v: Annotated): Annotation;
export declare function setα<T extends Annotated>(α: Annotation, v: T): void;
export declare function setjoinα(α: Annotation, v: Annotated): void;
export declare function setmeetα(α: Annotation, v: Annotated): void;
export declare enum Direction {
    Fwd = 0,
    Bwd = 1
}
export declare class Annotations {
    ann: Slice;
    direction: Direction;
    is(v: Annotated): Annotation;
    set(v: Annotated, α: Annotation): void;
    reset(direction: Direction): void;
    restrictTo(tvs: ExplValue[]): Slice;
}
export declare const __slice: Annotations;
