import { ExplValue } from "./DataValue";
import { Id, PrimOpTag, PrimValue, Value } from "./Value";
export declare type Unary<T, V> = (x: T) => (k: Id) => V;
export declare type Binary<T, U, V> = (x: T, y: U) => (k: Id) => V;
export declare class PrimOp<Tag extends PrimOpTag> extends Value<Tag> {
    name: string;
}
export declare class UnaryOp extends PrimOp<"UnaryOp"> {
    op: Unary<PrimValue, Value>;
}
export declare class BinaryOp extends PrimOp<"BinaryOp"> {
    op: Binary<PrimValue, PrimValue, Value>;
}
export declare function unary_<T extends PrimValue, V extends Value>(op: Unary<T, V>): ExplValue<UnaryOp>;
export declare function binary_<T extends PrimValue, U extends PrimValue, V extends Value>(op: Binary<T, U, V>): ExplValue<BinaryOp>;
export declare const unaryOps: Map<string, ExplValue<UnaryOp>>;
export declare const binaryOps: Map<string, ExplValue<BinaryOp>>;
