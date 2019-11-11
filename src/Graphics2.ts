import { List, Option, Pair } from "./BaseTypes"
import { initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { Num, Str, _ } from "./Value"

export type GraphicsElementTag = "Polyline" | "Rect" | "Group"

export class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
}

export class Group extends GraphicsElement<"Group"> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
   scale: Option<Scale> = _
   gs: List<GraphicsElement> = _
}

export class Rect extends GraphicsElement<"Rect"> {
   x: Num = _
   y: Num = _
   width: Num = _
   height: Num = _
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
