import { List, Option, Pair } from "./BaseTypes"
import { initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { Num, Str, _ } from "./Value"

export type GraphicsElementTag = "Polyline" | "Rect" | "Group"

// Every graphics element has coordinates and dimensions expressed in the coordinate system of its parent.
export class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
}

export class Group extends GraphicsElement<"Group"> {
   scale: Option<Scale> = _
   gs: List<GraphicsElement> = _
}

export class Rect extends GraphicsElement<"Rect"> {
   fill: Str = _
}

export class Polyline extends GraphicsElement<"Polyline"> {
   points: List<Pair<Num, Num>> = _
}

export class Scale extends DataValue<"Scale"> {
   x: Num = _
   y: Num = _
}

initDataType(GraphicsElement, [Group, Rect])
initDataType(Scale, [Scale])
