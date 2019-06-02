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

   toString(): string {
      return `Point(${this.x},${this.y})`
   }
}

export function point (x: Num, y: Num): Point {
   return make(Point, x, y)
}

export type GraphicsElementTag = "Graphic" | "Polyline" | "Polygon" | "Transform"

export abstract class GraphicsElement<Tag extends GraphicsElementTag = GraphicsElementTag> extends DataValue<Tag> {
}

export class Graphic extends GraphicsElement<"Graphic"> {
   gs: List<GraphicsElement> = _
}

export class Polyline extends GraphicsElement<"Polyline"> {
   points: List<Point> = _
}

// List of points must be closed.
export class Polygon extends GraphicsElement<"Polygon"> {
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

initDataType(GraphicsElement, [Polygon, Polyline, Transform, Graphic])
initDataType(LinearTransform, [Scale, Translate, Transpose])
initDataType(Point, [Point])
initDataType(Rect, [Rect])
