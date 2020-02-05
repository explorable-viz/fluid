import { DataValue } from "./DataValue";
import { Id, Persistent, Value } from "./Value";
export declare abstract class Bool extends DataValue<"Bool"> {
}
export declare class True extends Bool {
}
export declare function true_(): (k: Id) => Bool;
export declare class False extends Bool {
}
export declare function false_(): (k: Id) => Bool;
export declare abstract class List<T = Value> extends DataValue<"List"> {
    static fromArray<T extends Persistent>(x̅: T[]): List<T>;
    toArray(): T[];
    toArray_(x̅: T[]): void;
}
export declare class Nil<T = Value> extends List<T> {
    static is<T>(xs: List<T>): xs is Nil<T>;
}
export declare function nil<T>(): List<T>;
export declare class Cons<T = Value> extends List<T> {
    head: T;
    tail: List<T>;
    static is<T>(xs: List<T>): xs is Cons<T>;
}
export declare function cons<T extends Persistent>(head: T, tail: List<T>): Cons<T>;
export declare class Pair<T = Value, U = Value> extends DataValue<"Pair"> {
    fst: T;
    snd: U;
}
export declare function pair<T extends Persistent, U extends Persistent>(fst: T, snd: U): Pair<T, U>;
export declare abstract class Tree<T extends Persistent> extends DataValue<"Tree"> {
    toArray(): T[];
    toArray_(x̅: T[]): void;
}
export declare class Empty<T extends Persistent> extends Tree<T> {
    static is<T extends Persistent>(t: Tree<T>): t is Empty<T>;
}
export declare function empty<T extends Persistent>(): Empty<T>;
export declare class NonEmpty<T extends Persistent> extends Tree<T> {
    left: Tree<T>;
    t: T;
    right: Tree<T>;
    static is<T extends Persistent>(t: Tree<T>): t is NonEmpty<T>;
}
export declare function nonEmpty<T extends Persistent>(left: Tree<T>, t: T, right: Tree<T>): NonEmpty<T>;
export declare abstract class Option<T extends Persistent> extends DataValue<"Option"> {
}
export declare class None<T extends Persistent> extends Option<T> {
    static is<T extends Persistent>(o: Option<T>): o is None<T>;
}
export declare class Some<T extends Persistent> extends Option<T> {
    t: T;
    static is<T extends Persistent>(o: Option<T>): o is Some<T>;
}
export declare abstract class Ordering extends DataValue<"Ordering"> {
}
export declare class LT extends Ordering {
}
export declare class GT extends Ordering {
}
export declare class EQ extends Ordering {
}
export declare namespace BaseTypes {
    function initialise(): void;
}
