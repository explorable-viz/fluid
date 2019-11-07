import { List } from "./BaseTypes"
import { initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { Num, Str, _ } from "./Value"

// Basic graphical datatypes.

export class Point extends DataValue<"Point"> {
   x: Num = _
   y: Num = _

   toString(): string {
      return `Point(${this.x},${this.y})`
   }
}

export type GraphicsElementTag = "Rect" | "Graphic"

// Every graphics element has a coordinate expressed in the coordinate system of its parent, dimensions 
// expressed in its own coordinate system, and a scaling transformation which relates the two frames of reference.
// Thus (x, y) are in a sense "external" and (width, height) are "internal".
export class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
   scale: Scale = _
}

export class Graphic extends GraphicsElement<"Graphic"> {
   gs: List<GraphicsElement> = _
}

export class Rect extends GraphicsElement<"Rect"> {
   fill: Str = _
}

export class Scale extends DataValue<"Scale"> {
   x: Num = _
   y: Num = _
}

initDataType(GraphicsElement, [Graphic, Rect])
initDataType(Scale, [Scale])
initDataType(Point, [Point])
