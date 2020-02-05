import { Class } from "./util/Core";
import { Ord } from "./util/Ord";
import { ValueDelta } from "./Delta";
export declare const _: any;
export declare type DataValueTag = "Viewport" | "Circle" | "Group" | "Line" | "Marker" | "Orient" | "Polyline" | "Polymarkers" | "Scale" | "Translate" | "Text" | "Transform" | "Circle" | "Arrowhead" | "Bool" | "Closure" | "DataExpl" | "Elim" | "Match" | "Env" | "Expl" | "Expl.Def" | "Expl.RecDef" | "ExplValue" | "Expr" | "Expr.Def" | "List" | "Option" | "Ordering" | "Pair" | "Plug" | "Point" | "Expr.RecDef" | "Rect" | "Tree" | "Token";
export declare type LexemeTag = "Whitespace" | "SingleLineComment" | "Operator";
export declare type PrimOpTag = "UnaryOp" | "BinaryOp";
export declare type ValueTag = DataValueTag | LexemeTag | PrimOpTag | "Id" | "Num" | "Str";
export declare class Value<Tag extends ValueTag = ValueTag> {
    readonly __tag: Tag;
    __child(k: keyof this): Persistent;
    readonly __children: Persistent[];
}
export declare abstract class Id extends Value<"Id"> {
    tag<Tag extends string>(tag: Tag): TaggedId<Tag>;
}
declare class FunctionId extends Id {
    f: Function;
}
export declare class ApplicationId extends Id {
    k: MemoId;
    v: Persistent;
}
export declare type MemoId = FunctionId | ApplicationId;
export declare class TaggedId<Tag extends string> extends Id {
    k: Id;
    prop: Tag;
}
export declare function memoId(f: Function, v̅: Iterable<any>): MemoId;
export declare type Persistent = Value | boolean | string | number | Function;
export declare type PrimValue = Num | Str;
export declare function isPrim(v: Value): boolean;
export declare class Num extends Value<"Num"> {
    val: number;
    toString(): string;
}
export declare class Str extends Value<"Str"> implements Ord<Str> {
    val: string;
    toString(): string;
    leq(str: Str): boolean;
    eq(str: Str): boolean;
    geq(str: Str): boolean;
}
export declare function mergeInto(tgt: ValueDelta, src: ValueDelta): void;
export declare type MemoTable = Map<Persistent, Persistent | Map<Persistent, Object>>;
export interface Memoisable<T extends Persistent> {
    key: Persistent;
    call(args: Persistent[]): T;
}
export declare function memoCall<T extends Persistent>(memo: MemoTable, f: Memoisable<T>, v̅: Persistent[]): T;
export declare function make<T extends Value>(C: Class<T>, ...v̅: Persistent[]): T;
export declare function construct<T extends Value>(compare: boolean, tgt: T, v̅: Persistent[]): ValueDelta | null;
export declare function isField(prop: string): boolean;
export declare function fields<T>(v: T): (keyof T)[];
export declare function metadataFields(v: Value): string[];
export {};
