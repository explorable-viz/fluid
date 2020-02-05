import { Eq } from "./Eq";
export interface Ord<K extends Ord<K>> extends Eq<K> {
    leq(a: K): boolean;
}
export declare function eq<K extends Ord<K>>(a: K, b: K): boolean;
export declare type Comparator<T> = (x: T, y: T) => number;
export interface JoinSemilattice<T> {
    join(...ts: T[]): T;
    bot: T;
}
export interface MeetSemilattice<T> {
    meet(...ts: T[]): T;
    top: T;
}
export interface Lattice<T> extends JoinSemilattice<T>, MeetSemilattice<T> {
}
export interface BooleanLattice<T> extends Lattice<T> {
    negate(t: T): T;
}
