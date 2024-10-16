import { Expl } from "./Expl";
import { DataValueTag, Value } from "./Value";
export declare class DataValue<Tag extends DataValueTag = DataValueTag> extends Value<Tag> {
    readonly ctr: string;
    __child(k: keyof this): Value;
    readonly __children: Value[];
}
export declare class ExplValue<T extends Value = Value> extends DataValue<"ExplValue"> {
    t: Expl;
    v: T;
}
export declare function explValue<T extends Value = Value>(t: Expl, v: T): ExplValue<T>;
