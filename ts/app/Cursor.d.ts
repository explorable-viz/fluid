import { AClass, Class } from "../../src/util/Core";
import { List } from "../../src/BaseTypes";
import { DataValue, ExplValue } from "../../src/DataValue";
import { ValueDelta } from "../../src/Delta";
import { Expr } from "../../src/Expr";
import { Persistent, Value } from "../../src/Value";
import Def = Expr.Def;
import Let = Expr.Let;
import Prim = Expr.Prim;
import RecDef = Expr.RecDef;
export declare abstract class Cursor {
    abstract on: Value;
    abstract to<T extends DataValue>(C: Class<T>, k: keyof T): this;
    abstract at<T extends Value>(C: AClass<T>, f: (o: T) => void): Cursor;
    notAnnotated(): this;
    assert<T extends Value>(C: AClass<T>, pred: (v: T) => boolean): Cursor;
    αset(): this;
    αclear(): this;
    setα(): this;
    clearα(): this;
    treeNodeValue(): this;
    nth(n: number): this;
}
export declare class ExplValueCursor extends Cursor {
    ancestors: ExplValue[];
    readonly tv: ExplValue;
    constructor(ancestors: ExplValue[], tv: ExplValue);
    static descendant(prev: ExplValueCursor | null, tv: ExplValue): ExplValueCursor;
    static parent(child: ExplValueCursor): ExplValueCursor;
    readonly on: Value;
    to<T extends DataValue>(C: Class<T>, k: keyof T): this;
    toChild(n: number): ExplValueCursor;
    toChildOffset(tv: ExplValue, offset: number): ExplValueCursor;
    nextSibling(): ExplValueCursor;
    prevSibling(): ExplValueCursor;
    hasParent(): boolean;
    up(): ExplValueCursor;
    toBinaryArg1(opName: string): ExplValueCursor;
    toBinaryArg2(opName: string): ExplValueCursor;
    at<T extends Value>(C: AClass<T>, f: (o: T) => void): this;
    isChanged(s_ẟ: ValueDelta): ExplValueCursor;
    isUnchanged(): ExplValueCursor;
    isNew(): ExplValueCursor;
    toTerminal(): ExplValueCursor;
}
export declare class ExprCursor extends Cursor {
    readonly v: Value;
    constructor(v: Value);
    readonly on: Value;
    to<T extends DataValue>(C: Class<T>, prop: keyof T): this;
    constr_to<T extends DataValue>(C: Class<T>, prop: keyof T): ExprCursor;
    toCase<T extends DataValue>(C: Class<T>): ExprCursor;
    static defs(defs: List<Def>): Map<string, Let | Prim | RecDef>;
    toDef(x: string): ExprCursor;
    at<T extends Value>(C: AClass<T>, f: (o: T) => void): ExprCursor;
    var_(x: string): this;
    setNum(n: number): ExprCursor;
    setStr(str_: string): ExprCursor;
    constr_splice<T extends DataValue>(C: Class<T>, props: (keyof T)[], makeNode: (e̅: Expr[]) => Expr[]): ExprCursor;
    splice<T extends Value>(C: Class<T>, props: (keyof T)[], makeNode: (v̅: Persistent[]) => Persistent[]): ExprCursor;
}
