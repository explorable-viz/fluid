import { ExplValue } from "../DataValue";
import { Expl } from "../Expl";
import { GraphicsElement } from "../Graphics";
import { Pane } from "./Pane";
export declare module View {
    export let dimensions: (tg: ExplValue<GraphicsElement>) => [number, number];
    export let defaultDims: [number, number];
    export function initialise(resourceServerUrl: string): void;
    export function render(editor: Pane.Pane): void;
    export function existingView(tv: ExplValue): ExplValueView;
    abstract class View {
        render(): SVGSVGElement;
        abstract render_(): SVGSVGElement;
    }
    class ExplValueView extends View {
        tv: ExplValue;
        show_v: boolean;
        show_ts: boolean;
        t_visible: boolean;
        v_visible: boolean;
        assertValid(): void;
        constructor(tv: ExplValue, show_v: boolean, show_ts: boolean);
        initialise(): [Expl[], ExplValue | null];
        render_(): SVGSVGElement;
        toggleValue(): void;
        toggleExpl(): void;
    }
    export class ExplView extends View {
        t: Expl;
        bodyVisible: boolean;
        constructor(t: Expl);
        render_(): SVGSVGElement;
        appBody(): SVGElement;
    }
    export class ValueView extends View {
        tv: ExplValue;
        constructor(tv: ExplValue);
        render_(): SVGSVGElement;
    }
    export function valueView(tv: ExplValue): ValueView;
    export function view(tv: ExplValue, show_v: boolean, show_ts: boolean): ExplValueView;
    export function explView(t: Expl.Expl): View;
    export function splitExpls(t: Expl): Expl[];
    export function splitValue(tv: ExplValue): ExplValue;
    export {};
}
