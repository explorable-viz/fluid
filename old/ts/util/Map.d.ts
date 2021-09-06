export declare function toArray<K, V>(m: Map<K, V>): [K, V][];
export declare function filter<K, V>(m: Map<K, V>, pred: (v: V) => boolean): Map<K, V>;
export declare function map<K, V, U>(m: Map<K, V>, f: (u: V) => U): Map<K, U>;
export declare function extend<K, V>(m: Map<K, V>, kvs: [K, V][]): Map<K, V>;
export declare function intersectionWith<K, V, U>(m̅: Map<K, V>[], f: (v̅: V[]) => U): Map<K, U>;
export declare function keys<K, V>(m: Map<K, V>): Set<K>;
export declare function unionWith<K, V, U>(m̅: Map<K, V>[], f: (vs: V[]) => U): Map<K, U>;
export declare function union<K, V>(m̅: Map<K, V>[]): Map<K, V>;
export declare function values<K, V>(m: Map<K, V>): Set<V>;
