export interface Tag<T extends string> {
    typename: T;
}
export declare type Class<T = Object> = new (...args: any[]) => T;
export declare type AClass<T> = Function & {
    prototype: T;
};
export declare function classOf<T>(x: T): Class<T>;
export declare function className(o: Object): string;
export declare function as<U, T extends U>(x: U, C: AClass<T>): T;
export declare function asOpt<U, T extends U>(x: U, cls: AClass<T>): T;
export declare function assert(b: boolean, msg?: string, ...x̅: unknown[]): any;
export declare function absurd(msg?: string, ...x̅: unknown[]): any;
export declare function id<T>(x: T): T;
export declare function userError(msg: string, ...x̅: unknown[]): any;
export declare function notYetImplemented(): any;
export declare function abstractMethodError<T>(this_: Object): T;
export declare function __nonNull<T>(x: T | null | undefined, msg?: string): T;
export declare function __log<T>(x: T, msg?: (it: T) => string, transform?: (it: T) => T): T;
export declare function __check<T>(x: T, predicate: (it: T) => boolean): T;
