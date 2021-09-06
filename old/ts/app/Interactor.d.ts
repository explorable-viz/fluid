import { Instance as Tooltip } from "tippy.js";
import "tippy.js/dist/tippy.css";
import "tippy.js/themes/light-border.css";
import { Class } from "../util/Core";
import { DataValue, ExplValue } from "../DataValue";
import { Point, Rect } from "../Graphics";
import { ExplValueCursor } from "./Cursor";
import { Pane } from "./Pane";
declare abstract class Interactor<T extends DataValue, U extends SVGElement> {
    editor: Pane.Pane;
    C: Class<T>;
    tooltip: Tooltip;
    cursor: ExplValueCursor;
    element: U;
    propFocus: keyof T | null;
    constructor(editor: Pane.Pane, C: Class<T>, cursor: ExplValueCursor, element: U);
    propValues(g: T, props: (keyof T)[]): string;
    focusedProps<T extends DataValue>(tv: ExplValue<T>): (keyof T)[];
    abstract propFor(x_prop: number, y_prop: number): keyof T;
    onMouseMove(e: MouseEvent): void;
    onMouseOut(e: MouseEvent): void;
}
export declare class PointInteractor extends Interactor<Point, SVGElement> {
    constructor(editor: Pane.Pane, tp: ExplValueCursor, marker: SVGElement);
    propFor(x_prop: number, y_prop: number): keyof Point;
}
export declare class RectInteractor extends Interactor<Rect, SVGRectElement> {
    constructor(editor: Pane.Pane, tg: ExplValueCursor, r: SVGRectElement);
    propFor(x: number, y: number): keyof Rect;
}
export {};
