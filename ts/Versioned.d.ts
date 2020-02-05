import { Class } from "./util/Core";
import { Delta } from "./Delta";
import { Id, Persistent, Num, Str, Value } from "./Value";
export declare type Versioned<T> = Versioned_ & T;
export interface Versioned_ {
    __id: Id;
    __ẟ: Delta;
}
export declare function versioned<T extends Value>(v: T): v is Versioned<T>;
export declare function asVersioned<T extends Value>(v: T): Versioned<T>;
export declare function at<T extends Value>(C: Class<T>, ...v̅: Persistent[]): (k: Id) => Versioned<T>;
export declare function create<T extends Value>(C: Class<T>, ...v̅: Persistent[]): (k: Id) => Versioned<T>;
export declare function reset<T extends Value>(v: Value, C: Class<T>, ...v̅: Persistent[]): void;
export declare class Extern extends Id {
    id: number;
}
export declare const ν: () => Extern;
export declare function num(val: number): (k: Id) => Versioned<Num>;
export declare function str(val: string): (k: Id) => Versioned<Str>;
export declare function newRevision(): void;
export declare type MemoFunType<T extends Persistent> = (...v̅: Persistent[]) => T;
export declare function memo<T extends Persistent>(f: MemoFunType<T>, ...v̅: Persistent[]): T;
