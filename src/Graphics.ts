import * as THREE from "three"
import { AnnNumber } from "./app/Reflect"
import { Annotated, Annotation, ann } from "./util/Annotated"
import { absurd, as } from "./util/Core"
import { Persistent, make } from "./util/Persistent"
import { Cons, List } from "./BaseTypes"

// Basic graphical datatypes.

export class Rect extends Annotated {
   width: AnnNumber
   height: AnnNumber

   constructor_ (α: Annotation, width: AnnNumber, height: AnnNumber): void {
      this.α = α
      this.width = width
      this.height = height
   }
}

export function rect (width: AnnNumber, height: AnnNumber): Rect {
   return make(Rect, ann.bot, width, height)
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
   elem: GraphicsElement

   constructor_ (α: Annotation, vec: Point, elem: GraphicsElement): void {
      this.α = α
      this.vec = vec
      this.elem = elem
   }
}

// Swaps x and y. Could subsume by a more general notion of reflection.
export class Transpose extends GraphicsElement {
   elem: GraphicsElement

   constructor_ (α: Annotation, elem: GraphicsElement): void {
      this.α = α
      this.elem = elem
   }
}

type Transform = (p: THREE.Vector2) => THREE.Vector2

export class Canvas3D {
   transforms: Transform[] // stack of active linear transformations (so far only translation)

   constructor () {
      this.transforms = [x => x]
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
         const transform: Transform = this.transforms.slice(-1)[0]
         this.transforms.push(p => {
            let {x, y}: THREE.Vector2 = transform(p)
            return new THREE.Vector2(x + elem.vec.x.n, y + elem.vec.y.n)
         })
         const objects: THREE.Object3D[] = this.objects3D(elem.elem)
         this.transforms.pop()
         return objects
      } else
      if (elem instanceof Transpose) {
         const transform: Transform = this.transforms.slice(-1)[0]
         this.transforms.push(p => {
            let {x, y}: THREE.Vector2 = transform(p)
            return new THREE.Vector2(y, x)
         })
         const objects: THREE.Object3D[] = this.objects3D(elem.elem)
         this.transforms.pop()
         return objects
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
         const point: Point = as(points.head, Point),
               transform2: Transform = this.transforms.slice(-1)[0],
               {x, y}: THREE.Vector2 = transform2(new THREE.Vector2(point.x.n, point.y.n))
         geometry.vertices.push(new THREE.Vector3(x, y, 0))
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
