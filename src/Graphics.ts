import { initDataType } from "./DataType"
import { DataValue } from "./DataValue"
import { Num, _, make } from "./Value"
import { List } from "./BaseTypes"

// Basic graphical datatypes.

export class Rect extends DataValue<"Rect"> {
   width: Num = _
   height: Num = _
}

export function rect (width: Num, height: Num): Rect {
   return make(Rect, width, height)
}

export class Point extends DataValue<"Point"> {
   x: Num = _
   y: Num = _
}

export function point (x: Num, y: Num): Point {
   return make(Point, x, y)
}

export type GraphicsElementTag = "Graphic" | "PathStroke" | "RectFill" | "Transform"

export abstract class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
}

export class Graphic extends GraphicsElement<"Graphic"> {
   gs: List<GraphicsElement> = _
}

export class PathStroke extends GraphicsElement<"PathStroke"> {
   points: List<Point> = _
}

// TODO: generalise to any (closed) path.
export class RectFill extends GraphicsElement<"RectFill"> {
   points: List<Point> = _
}

export class Transform extends GraphicsElement<"Transform"> {
   t: LinearTransform = _
   g: GraphicsElement = _
}

export type LinearTransformTag = "Scale" | "Translate" | "Transpose"

export abstract class LinearTransform<Tag extends LinearTransformTag = LinearTransformTag> extends DataValue<Tag> {
}

export class Scale extends LinearTransform<"Scale"> {
   x: Num = _
   y: Num = _
}

export class Translate extends LinearTransform<"Translate"> {
   x: Num = _
   y: Num = _
}

// Swaps x and y. Could subsume by a more general notion of reflection.
export class Transpose extends LinearTransform<"Transpose"> {
}

initDataType(GraphicsElement, [PathStroke, RectFill, Transform, Graphic])
initDataType(LinearTransform, [Scale, Translate, Transpose])
initDataType(Point, [Point])
initDataType(Rect, [Rect])
