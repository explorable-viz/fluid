export declare function flatten<T>(x̅̅: T[][]): T[];
export declare function counts<T>(x̅: T[]): Map<T, number>;
export declare function zip<T, U>(x̅: T[], y̅: U[]): [T, U][];
export declare function zipWith<T, U, V>(f: (t: T, u: U) => V): (x̅: T[], y̅: U[]) => V[];
export declare function includes<T>(x̅: T[], y̅: T[]): boolean;
export declare function eq<T>(x̅: T[], y̅: T[]): boolean;
export declare function nth<T>(x̅: T[], n: number): T;
export declare function last<T>(x̅: T[]): T;
