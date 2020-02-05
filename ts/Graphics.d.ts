import { List } from "./BaseTypes";
import { DataValue } from "./DataValue";
import { Num, Str } from "./Value";
export declare class Point extends DataValue<"Point"> {
    x: Num;
    y: Num;
}
export declare class Orient extends DataValue<"Orient"> {
}
export declare class Horiz extends Orient {
}
export declare class Vert extends Orient {
}
export declare type GraphicsElementTag = "Circle" | "Group" | "Line" | "Polyline" | "Polymarkers" | "Rect" | "Text" | "Viewport";
export declare class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
}
export declare class Circle extends GraphicsElement<"Circle"> {
    x: Num;
    y: Num;
    radius: Num;
    fill: Str;
}
export declare class Group extends GraphicsElement<"Group"> {
    gs: List<GraphicsElement>;
}
export declare class Line extends GraphicsElement<"Line"> {
    p1: Point;
    p2: Point;
    stroke: Str;
    strokeWidth: Num;
}
export declare class Polyline extends GraphicsElement<"Polyline"> {
    points: List<Point>;
    stroke: Str;
    strokeWidth: Num;
}
export declare class Polymarkers extends GraphicsElement<"Polymarkers"> {
    points: List<Point>;
    markers: List<GraphicsElement>;
}
export declare class Rect extends GraphicsElement<"Rect"> {
    x: Num;
    y: Num;
    width: Num;
    height: Num;
    fill: Str;
}
export declare class Text extends GraphicsElement<"Text"> {
    x: Num;
    y: Num;
    str: Str;
    anchor: Str;
    baseline: Str;
}
export declare class Viewport extends GraphicsElement<"Viewport"> {
    x: Num;
    y: Num;
    width: Num;
    height: Num;
    fill: Str;
    margin: Num;
    scale: Transform;
    translate: Transform;
    g: GraphicsElement;
}
export declare type TransformTag = "Scale" | "Translate";
export declare class Transform<Tag extends TransformTag = TransformTag> extends DataValue<Tag> {
}
export declare class Scale extends Transform<"Scale"> {
    x: Num;
    y: Num;
}
export declare class Translate extends Transform<"Translate"> {
    x: Num;
    y: Num;
}
export declare type MarkerTag = "Arrowhead";
export declare class Marker<Tag extends MarkerTag = MarkerTag> extends DataValue<Tag> {
}
export declare class Arrowhead extends Marker<"Arrowhead"> {
}
