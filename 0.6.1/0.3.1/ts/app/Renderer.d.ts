import { Class } from "../util/Core";
import { strings } from "../Expr";
import { Marker } from "../Graphics";
import { Value } from "../Value";
import { SVG } from "./Core";
import "./styles.css";
export declare const svg: SVG;
declare type Dimensions = {
    width: number;
    height: number;
};
export declare const __dimensions: Map<SVGElement, Dimensions>;
export declare function arrow(ẟ_style: DeltaStyle): SVGElement;
export declare function border(x: number, y: number, width: number, height: number, stroke: string, dashed: boolean): SVGRectElement;
export declare function addBorder_changed(g: SVGSVGElement): SVGSVGElement;
export declare function addBorder_focus(g: SVGSVGElement): SVGSVGElement;
export declare function bracket(gs: SVGElement[], ẟ_style: DeltaStyle): SVGSVGElement;
export declare function centreDot(ẟ_style: DeltaStyle): SVGElement;
export declare function comma(ẟ_style: DeltaStyle): SVGElement;
export declare function connector(g1: SVGSVGElement, g2: SVGSVGElement): SVGElement;
export declare function delimit(delimiter: () => SVGElement, ...gs: SVGElement[]): SVGElement[];
export declare function edge_left(g: SVGSVGElement): SVGSVGElement;
export declare function edge_bottom(g: SVGSVGElement): SVGSVGElement;
export declare function ellipsis(ẟ_style: DeltaStyle): SVGElement;
export declare function group(): SVGGElement;
export declare function horiz(...gs: SVGElement[]): SVGSVGElement;
export declare function horizSpace(...gs: SVGElement[]): SVGSVGElement;
export declare function keyword(str: keyof typeof strings, ẟ_style: DeltaStyle): SVGElement;
export declare function line(x1: number, y1: number, x2: number, y2: number, stroke: string, strokeWidth: number): SVGLineElement;
export declare function lineRounded(x1: number, y1: number, x2: number, y2: number, stroke: string, strokeWidth: number): SVGLineElement;
export declare function marker(C: Class<Marker>, colour: string): SVGMarkerElement;
export declare type MarkerFactory = (colour: string) => SVGMarkerElement;
export declare function markerEnsureDefined(root: SVGSVGElement, C: Class<Marker>, colour: string): string;
export declare function marker_arrowhead(colour: string): SVGMarkerElement;
export declare function circle(x: number, y: number, radius: number, stroke: string, fill: string, createdBy: Function): SVGCircleElement;
export declare function parenthesise(g: SVGElement, ẟ_style: DeltaStyle): SVGSVGElement;
export declare function parenthesiseIf(parens: boolean, g: SVGSVGElement, ẟ_style: DeltaStyle): SVGSVGElement;
export declare function polyline(p̅: [number, number][], stroke: string, strokeWidth: number): SVGPolylineElement;
export declare function rect(x: number, y: number, width: number, height: number, stroke: string, fill: string, createdBy: Function): SVGRectElement;
export declare function round(n: number): string;
export declare function shading(g: SVGSVGElement, fill: string): SVGSVGElement;
export declare function space(): SVGElement;
export declare function svgElement(overflow: boolean, x: number, y: number, width: number, height: number, defs: boolean, createdBy: Function): SVGSVGElement;
export declare function svgElement_inverted(w: number, h: number): [SVGSVGElement, SVGGElement];
export declare function svgRootElement(w: number, h: number): SVGSVGElement;
export declare function text(str: string, ẟ_style: DeltaStyle): SVGTextElement;
export declare function textElement_graphical(x: number, y: number, fontSize: number, str: string): SVGTextElement;
export declare function unimplemented(v: Value): SVGSVGElement;
export declare function vert(...gs: SVGElement[]): SVGSVGElement;
export declare enum DeltaStyle {
    New = "new",
    Changed = "changed",
    Unchanged = "unchanged"
}
export declare function deltaStyle(v: Value): DeltaStyle;
export {};
