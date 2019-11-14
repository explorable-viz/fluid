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
   scale: Option<Transform> = _
   translate: Option<Transform> = _ // scaling applies to translates coordinates
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
   scale: Option<Transform> = _
   points: List<Pair<Num, Num>> = _
   stroke: Str = _
   marker: Option<Marker> = _
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

export type MarkerTag = "Tick" | "Circle"

export class Marker<Tag extends MarkerTag = MarkerTag> extends DataValue<Tag> {
}

export class Tick extends Marker<"Tick"> {
}

export class Circle extends Marker<"Circle"> {   
}

initDataType(GraphicsElement, [Group, Polyline, Rect])
initDataType(Transform, [Scale, Translate])
initDataType(Marker, [Tick, Circle])
