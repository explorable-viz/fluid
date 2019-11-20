import { List, Pair } from "./BaseTypes"
import { initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { Num, Str, _ } from "./Value"

// Isomorphic to Bool
export class Orient extends DataValue<"Orient"> {
}

export class Horiz extends Orient {
}

export class Vert extends Orient {
}

export type GraphicsElementTag = "Circle" | "Group" | "Line" | "Polyline" | "Polymarkers" | "Rect" | "Text" | "Viewport"

export class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
}

export class Circle extends GraphicsElement<"Circle"> {   
   x: Num = _
   y: Num = _
   radius: Num = _
   fill: Str = _
}

export class Group extends GraphicsElement<"Group"> {
   gs: List<GraphicsElement> = _
}

export class Line extends GraphicsElement<"Line"> {
   p1: Pair<Num, Num> = _
   p2: Pair<Num, Num> = _
   stroke: Str = _
   strokeWidth: Num = _
}

export class Polyline extends GraphicsElement<"Polyline"> {
   points: List<Pair<Num, Num>> = _
   stroke: Str = _
   strokeWidth: Num = _
}

export class Polymarkers extends GraphicsElement<"Polymarkers"> {
   points: List<Pair<Num, Num>> = _
   markers: List<GraphicsElement> = _
}

export class Rect extends GraphicsElement<"Rect"> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
   fill: Str = _
}

export class Text extends GraphicsElement<"Text"> {
   x: Num = _
   y: Num = _
   str: Str = _
   anchor: Str = _   // SVG text-anchor
   baseline: Str = _ // SVG alignment-baseline
}

export class Viewport extends GraphicsElement<"Viewport"> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
   fill: Str = _
   scale: Transform = _
   translate: Transform = _ // scaling applies to translated coordinates
   gs: List<GraphicsElement> = _
}

export type TransformTag = "Scale" | "Translate"

export class Transform<Tag extends TransformTag = TransformTag> extends DataValue<Tag> {
}

export class Scale extends Transform<"Scale"> {
   x: Num = _
   y: Num = _
}

export class Translate extends Transform<"Translate"> {
   x: Num = _
   y: Num = _
}

export type MarkerTag = "Arrowhead"

export class Marker<Tag extends MarkerTag = MarkerTag> extends DataValue<Tag> {
}

export class Arrowhead extends Marker<"Arrowhead"> {   
}

// A "locally scale-invariant" graphics element. SVG markers are similar, but we can use this for elements that
// change at every point of a polymarkers element (for example).
export class Unscaled extends DataValue<"Unscaled"> {
   width: Num = _
   height: Num = _
   g: GraphicsElement = _
}

initDataType(Orient, [Horiz, Vert])
initDataType(GraphicsElement, [Circle, Group, Line, Polyline, Polymarkers, Rect, Text, Viewport])
initDataType(Transform, [Scale, Translate])
initDataType(Marker, [Arrowhead])
