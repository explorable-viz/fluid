import { Instance as Tooltip, Placement } from "tippy.js";
import { Direction, Slice } from "../Annotation";
import { ExplValue } from "../DataValue";
import { Env } from "../Env";
import { Expr } from "../Expr";
import { ExplValueCursor } from "./Cursor";
import "./styles.css";
export declare module Pane {
    function initialise(resourceServerUrl: string): void;
    interface Listener {
        onBwdSlice(editor: Pane, externDeps: Slice): void;
    }
    class Pane {
        listener: Listener;
        rootPane: SVGSVGElement;
        tooltips: Set<Tooltip>;
        tooltipPlacement: Placement;
        ρ_external: Env;
        ρ_imports: Env;
        e: Expr;
        tv: ExplValue;
        here: ExplValueCursor;
        direction: Direction;
        slice: Slice;
        constructor(listener: Listener, appRoot: HTMLElement, [width, height]: [number, number], tooltipPlacement: Placement, ρ_external: Env, ρ_imports: Env, e: Expr);
        readonly ρ: Env;
        visibleTooltips(): Tooltip[];
        initialise(): void;
        bwdSlice(setNeeded: () => void): void;
        fwdSlice(externDeps: Slice): void;
        render(): void;
        onEdit(): void;
        onViewChange(): void;
    }
}
