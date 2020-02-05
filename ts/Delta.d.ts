import { Ord } from "./util/Ord";
import { Persistent, Value } from "./Value";
export interface ValueDelta {
    [prop: string]: {
        before: Persistent;
        after: Persistent;
    };
}
export declare function leq(s1: ValueDelta, s2: ValueDelta): boolean;
export declare class Deltas {
    ẟ̅: Map<Value, Delta>;
    readonly size: number;
    changed(v: Value, s_ẟ: ValueDelta): void;
    reclassified(v: Value): void;
    created(v: Value): void;
    clear(): void;
}
export declare const __deltas: Deltas;
export declare abstract class Delta implements Ord<Delta> {
    abstract leq(ẟ: Delta): boolean;
    eq(ẟ: Delta): boolean;
}
export declare class New extends Delta {
    constructor();
    leq(ẟ: Delta): boolean;
}
export declare class Change extends Delta {
    changed: ValueDelta;
    constructor(changed: ValueDelta);
    leq(ẟ: Delta): boolean;
    hasChanged(prop: string): boolean;
}
export declare class Reclassify extends Delta {
    constructor();
    leq(ẟ: Delta): boolean;
}
