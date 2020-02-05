import { AClass, Class } from "./util/Core";
import { DataValue } from "./DataValue";
import { Expl } from "./Expl";
import { Expr } from "./Expr";
import { DataElim } from "./Match";
import { PrimValue, Str } from "./Value";
export declare class PrimType {
    name: Str;
    C: Class<PrimValue>;
    constructor(name: Str, C: Class<PrimValue>);
}
export declare class DataType {
    name: Str;
    elimC: Class<DataElim>;
    ctrs: Map<string, Ctr>;
    exprC̅: Map<string, Class<Expr.DataExpr>>;
    explC̅: Map<string, Class<Expl.DataExpl>>;
    constructor(name: Str, elimC: Class<DataElim>, ctrs: Map<string, Ctr>, exprC̅: Map<string, Class<Expr.DataExpr>>, explC̅: Map<string, Class<Expl.DataExpl>>);
}
export declare class Ctr {
    C: Class<DataValue>;
    f̅: string[];
    constructor(C: Class<DataValue>, f̅: string[]);
    readonly arity: number;
    readonly c: string;
}
export declare function ctrFor(c: string): Ctr;
export declare function explClass(C: Class<DataValue>): Class<Expl.DataExpl>;
export declare function exprClass(C: Class<DataValue>): Class<Expr.DataExpr>;
export declare function valueClass(C: Class<Expr.DataExpr>): Class<DataValue>;
export declare const types: Map<string, DataType | PrimType>;
export declare const ctrToDataType: Map<string, DataType>;
export declare const elimToDataType: Map<string, DataType>;
export declare function initDataType<T extends DataValue>(D: AClass<T>, C̅: Class<T>[]): void;
