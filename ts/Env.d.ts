import { DataValue, ExplValue } from "./DataValue";
import { Str } from "./Value";
export declare abstract class Env extends DataValue<"Env"> {
    get(k: Str): ExplValue | undefined;
    has(k: Str): boolean;
    static singleton(k: Str, tv: ExplValue): ExtendEnv;
    concat(ρ: Env): Env;
    values(): ExplValue[];
}
export declare class EmptyEnv extends Env {
}
export declare function emptyEnv(): EmptyEnv;
export declare class ExtendEnv extends Env {
    ρ: Env;
    k: Str;
    tv: ExplValue;
}
export declare function extendEnv(ρ: Env, k: Str, tv: ExplValue): ExtendEnv;
