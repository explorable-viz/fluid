import { List, Pair } from "./BaseTypes";
import { Env, ExtendEnv } from "./Env";
import { Expr } from "./Expr";
import "./Graphics";
import { PrimValue, Str } from "./Value";
export declare namespace Module {
    let resourceServerUrl: string;
    let prelude: Env;
    let graphics: Env;
    function initialise(resourceServerUrl: string): void;
}
export declare function loadFile(folder: string, file: string): string;
export declare function loadModule(ρ: Env, file: string): Env;
export declare function openWithImports(file: string, ...modules: Env[]): [Env, Expr];
export declare function openDatasetAs(file: string, x: string): ExtendEnv;
export declare function parseWithImports(src: string, ...modules: Env[]): [Env, Expr];
export declare function successfulParse(str: string): Expr;
export declare type Record = List<Pair<Str, PrimValue>>;
export declare function bindDataset(ρ: Env, vs: Object[], x: string): ExtendEnv;
