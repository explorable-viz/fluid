export declare function diff<T>(x̅: Set<T>, y̅: Set<T>): Set<T>;
export declare function every<T>(x̅: Set<T>, pred: (x: T) => boolean): boolean;
export declare function filter<T>(x̅: Set<T>, pred: (x: T) => boolean): Set<T>;
export declare function map<T, U>(x̅: Set<T>, f: (x: T) => U): Set<U>;
export declare function some<T>(x̅: Set<T>, pred: (x: T) => boolean): boolean;
export declare function union<T>(...x̅̅: Set<T>[]): Set<T>;
export declare function intersection<T>(x̅̅: Set<T>, y̅: Set<T>): Set<T>;
