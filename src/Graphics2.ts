import { Constr, Num, _, make } from "./Value2"
import { List } from "./BaseTypes2"

// Basic graphical datatypes.

export class Rect extends Constr<"Rect"> {
   width: Num = _
   height: Num = _
}

export function rect (width: Num, height: Num): Rect {
   return make(Rect, width, height)
}

export class Point extends Constr<"Point"> {
   x: Num = _
   y: Num = _
}

export function point (x: Num, y: Num): Point {
   return make(Point, x, y)
}

export abstract class GraphicsElement<Tag extends string = any> extends Constr<Tag> {
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

export abstract class LinearTransform<Tag extends string = any> extends Constr<Tag> {
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
