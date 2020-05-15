import { Placement } from "tippy.js";
import { Slice } from "../Annotation";
import { Env } from "../Env";
import { Expr } from "../Expr";
import { Pane } from "./Pane";
export declare class PaneCoordinator implements Pane.Listener {
    appRoot: HTMLElement;
    ρ_external: Env;
    panes: Set<Pane.Pane>;
    constructor(appRoot: HTMLElement, ρ_external: Env);
    addPane(ρ: Env, e: Expr, tooltipPlacement?: Placement): Pane.Pane;
    removePane(pane: Pane.Pane): void;
    onBwdSlice(editor: Pane.Pane, externDeps: Slice): void;
}
