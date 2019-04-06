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

export class Graphic extends GraphicsElement {
   elems: List<GraphicsElement>

   constructor_ (α: Annotation, elems: List<GraphicsElement>): void {
      this.α = α
      this.elems = elems
   }
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

export class Translate extends GraphicsElement {
   vec: Point

   constructor_ (α: Annotation, vec: Point): void {
      this.α = α
      this.vec = vec
   }
}

export class Canvas3D {
   vec: THREE.Vector2 // current linear transformation (so far only translation)

   constructor () {
      this.vec = new THREE.Vector2(0, 0)
   }

   objects3D (elem: GraphicsElement): THREE.Object3D[] {
      if (elem instanceof Graphic) {   
         const objects: THREE.Object3D[] = []
         for (let elems: List<GraphicsElement> = elem.elems; Cons.is(elems); elems = elems.tail) {
            objects.push(...this.objects3D(elems.head))
         }
         return objects
      }
      if (elem instanceof PathStroke) {
         return this.pathStroke(elem.points)
      } else
      if (elem instanceof RectFill) {
         return this.rectFill(elem.points)
      } else
      if (elem instanceof Translate) {
         this.vec.setX(this.vec.x + elem.vec.x.n)
         this.vec.setY(this.vec.y + elem.vec.y.n)
         return []
      } else {
         return absurd()
      }
   }

   // Assume closed path for now.
   pathStroke (points: List<Point>): THREE.Object3D[] {
      const stroke: THREE.LineLoop = new THREE.LineLoop(
         this.newPathGeometry(points),
         new THREE.LineBasicMaterial({ 
            color: 0x000000 
         })
      )
      return [stroke, ...pointHighlights(points)]
   }

   rectFill (rect_path: List<Point>): THREE.Object3D[] {
      const geometry: THREE.Geometry = this.newPathGeometry(rect_path)
      geometry.faces.push(new THREE.Face3(0,1,2))
      geometry.faces.push(new THREE.Face3(2,3,0))
      return [new THREE.Mesh(
         geometry, 
         new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
      )]
   }

   newPathGeometry (points: List<Point>): THREE.Geometry {
      const geometry: THREE.Geometry = new THREE.Geometry
      while (Cons.is(points)) {
         const point: Point = as(points.head, Point)
         geometry.vertices.push(new THREE.Vector3(point.x.n + this.vec.x, point.y.n + this.vec.y, 0))
         points = points.tail
      }
      return geometry
   }   
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
      if (!point.x.α || !point.y.α) {
         console.log(point)
         highlights.push(circle(point, 0.5))
      }
   }
   return highlights
}
