import { List, Option } from "./BaseTypes"
import { DataValue } from "./DataValue"
import { Num, Str, _ } from "./Value"

// Basic graphical datatypes.

export class Rect extends DataValue<"Rect"> {
   width: Num = _
   height: Num = _
}

export class Point extends DataValue<"Point"> {
   x: Num = _
   y: Num = _

   toString(): string {
      return `Point(${this.x},${this.y})`
   }
}

export type GraphicsElementTag = "Graphic" | "Marker" | "Polyline" | "Polygon" | "Text" | "Translate"

export class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
}

export class Graphic extends GraphicsElement<"Graphic"> {
   gs: List<GraphicsElement> = _
}

// Isomorphic to unit for now, but will extend later.
export class Marker extends GraphicsElement<"Marker"> {
}

export class Polyline extends GraphicsElement<"Polyline"> {
   points: List<Point> = _
   marker: Option<Marker> = _
}

export class Polygon extends GraphicsElement<"Polygon"> {
   points: List<Point> = _
   stroke: Str = _
   fill: Str = _
}

export class Text extends GraphicsElement<"Text"> {
   x: Num = _
   y: Num = _
   str: Str = _
}

// Omit scaling, rotation, etc for now; would require externalisation to SVG to handle text properly.
export class Translate extends GraphicsElement<"Translate"> {
   x: Num = _
   y: Num = _
   g: GraphicsElement = _
}
