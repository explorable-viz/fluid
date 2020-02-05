import { BooleanLattice } from "./Ord";
declare abstract class LatticeImpl<T> implements BooleanLattice<T> {
    abstract bot: T;
    abstract top: T;
    join(...t̅: T[]): T;
    meet(...t̅: T[]): T;
    abstract join2(t1: T, t2: T): T;
    abstract meet2(t1: T, t2: T): T;
    abstract negate(t: T): T;
}
export declare class BoolLattice extends LatticeImpl<boolean> {
    bot: boolean;
    top: boolean;
    join2(b1: boolean, b2: boolean): boolean;
    meet2(b1: boolean, b2: boolean): boolean;
    negate(b: boolean): boolean;
}
export declare const bool_: BooleanLattice<Annotation>;
export declare type Annotation = boolean;
export {};
