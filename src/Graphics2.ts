import { List, Option } from "./BaseTypes"
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

// Every graphics element has coordinates and dimensions expressed in the coordinate system of its parent.
export class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
   scale: Option<Scale> = _
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
