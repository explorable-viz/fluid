import { Num, Value, _, make } from "./Value2"
import { List } from "./BaseTypes2"

// Basic graphical datatypes.

export class Rect extends Value {
   width: Num = _
   height: Num = _
}

export function rect (width: Num, height: Num): Rect {
   return make(Rect, width, height)
}

export class Point extends Value {
   x: Num = _
   y: Num = _
}

export function point (x: Num, y: Num): Point {
   return make(Point, x, y)
}

export abstract class GraphicsElement extends Value {
}

export class Graphic extends GraphicsElement {
   gs: List<GraphicsElement> = _
}

export class PathStroke extends GraphicsElement {
   points: List<Point> = _
}

// TODO: generalise to any (closed) path.
export class RectFill extends GraphicsElement {
   points: List<Point> = _
}

export abstract class LinearTransform extends Value {
}

export class Scale extends LinearTransform {
   x: Num = _
   y: Num = _
}

export class Translate extends LinearTransform {
   x: Num = _
   y: Num = _
}

// Swaps x and y. Could subsume by a more general notion of reflection.
export class Transpose extends LinearTransform {
}

export class Transform extends GraphicsElement {
   t: LinearTransform = _
   g: GraphicsElement = _
}
