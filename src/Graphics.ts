import * as THREE from "three"
import { AnnNumber } from "./app/Reflect"
import { Annotated, Annotation, ann } from "./util/Annotated"
import { absurd, as } from "./util/Core"
import { Persistent, make } from "./util/Persistent"
import { Cons, List } from "./BaseTypes"

// Basic graphical datatypes.

export class Rect extends Annotated {
   x: AnnNumber
   y: AnnNumber
   width: AnnNumber
   height: AnnNumber

   constructor_ (α: Annotation, x: AnnNumber, y: AnnNumber, width: AnnNumber, height: AnnNumber): void {
      this.α = α
      this.x = x
      this.y = y
      this.width = width
      this.height = height
   }
}

export function rect (x: AnnNumber, y: AnnNumber, width: AnnNumber, height: AnnNumber): Rect {
   return make(Rect, ann.bot, x, y, width, height)
}

export class Point extends Annotated {
   x: AnnNumber
   y: AnnNumber

   constructor_ (α: Annotation, x: AnnNumber, y: AnnNumber): void {
      this.α = α
      this.x = x
      this.y = y
   }
}

export function point (x: AnnNumber, y: AnnNumber): Point {
   return make(Point, ann.bot, x, y)
}

export abstract class GraphicsElement extends Annotated {
   abstract constructor_ (...v̅: Persistent[]): void
}

export class PathStroke extends GraphicsElement {
   points: List<Point>

   constructor_ (α: Annotation, points: List<Point>): void {
      this.α = α
      this.points = points
   }
}

// TODO: generalise to any (closed) path.
export class RectFill extends GraphicsElement {
   points: List<Point>

   constructor_ (α: Annotation, points: List<Point>): void {
      this.α = α
      this.points = points
   }
}

export function objects3D (elem: GraphicsElement): THREE.Object3D[] {
   if (elem instanceof PathStroke) {
      return pathStroke(elem.points)
   } else
   if (elem instanceof RectFill) {
      return rectFill(elem.points)
   } else {
      return absurd()
   }
}

function newPathGeometry (points: List<Point>): THREE.Geometry {
   const geometry: THREE.Geometry = new THREE.Geometry
   while (Cons.is(points)) {
      const point: Point = as(points.head, Point)
      geometry.vertices.push(new THREE.Vector3(point.x.n, point.y.n, 0))
      points = points.tail
   }
   return geometry
}

function circle (pos: Point, radius: number): THREE.Object3D {
   const material = new THREE.LineBasicMaterial({ color: 0x0000ff }),
         geometry = new THREE.CircleGeometry( radius, 64 )
   geometry.vertices.shift() // remove center vertex
   const circle: THREE.LineLoop = new THREE.LineLoop(geometry, material)
   circle.position.x = pos.x.n
   circle.position.y = pos.y.n
   return circle
}

function pointHighlights (points: List<Point>): THREE.Object3D[] {
   const highlights: THREE.Object3D[] = []
   for (; Cons.is(points); points = points.tail) {
      const point: Point = points.head
      if (!point.α) {
         console.log(point)
         highlights.push(circle(point, 10))
      }
   }
   return highlights
}

function pathStroke (points: List<Point>): THREE.Object3D[] {
   const stroke: THREE.Line = new THREE.Line(
      newPathGeometry(points),
      new THREE.LineBasicMaterial({ 
         color: 0x000000 
      })
   )
   return [stroke, ...pointHighlights(points)]
}

function rectFill (rect_path: List<Point>): THREE.Object3D[] {
   const geometry: THREE.Geometry = newPathGeometry(rect_path)
   geometry.faces.push(new THREE.Face3(0,1,2))
   geometry.faces.push(new THREE.Face3(2,3,0))
   return [new THREE.Mesh(
      geometry, 
      new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
   )]
}
