import * as THREE from "three"
import { Annotated, Annotation, ann } from "./util/Annotated"
import { absurd, as } from "./util/Core"
import { Persistent, make } from "./util/Persistent"
import { Cons, List, cons } from "./BaseTypes"

// Basic graphical datatypes.

export class Rect extends Annotated {
   x: Number
   y: Number
   width: Number
   height: Number

   constructor_ (α: Annotation, x: Number, y: Number, width: Number, height: Number) {
      this.α = α
      this.x = x
      this.y = y
      this.width = width
      this.height = height
   }
}

export function rect (x: Number, y: Number, width: Number, height: Number): Rect {
   return make(Rect, ann.bot, x, y, width, height)
}

export class Point extends Annotated {
   x: Number
   y: Number

   constructor_ (α: Annotation, x: Number, y: Number) {
      this.α = α
      this.x = x
      this.y = y
   }
}

export function point (x: Number, y: Number): Point {
   return make(Point, ann.bot, x, y)
}

// We don't have anything like typeclasses yet.
export function objects (elem: Persistent): THREE.Object3D[] {
   if (elem instanceof Rect) {
      return [rect_fill(elem), rect_stroke(elem)]
   } else 
   if (elem instanceof List/*<Point>*/) { // silent "any"
      return [path_stroke(elem)]
   } else {
      return absurd()
   }
}

function newPathGeometry (points: List<Point>): THREE.Geometry {
   const geometry: THREE.Geometry = new THREE.Geometry
   while (Cons.is(points)) {
      const point: Point = as(points.head, Point)
      geometry.vertices.push(new THREE.Vector3(point.x.valueOf(), point.y.valueOf(), 0))
      points = points.tail
   }
   return geometry
}

function path_stroke (points: List<Point>): THREE.Object3D {
   return new THREE.Line(
      newPathGeometry(points), 
      new THREE.LineBasicMaterial({ 
         color: 0x000000 
      })
   )
}

function rect_path (rect: Rect): List<Point> {
   return List.fromArray([
      point(rect.x, rect.y),
      point(rect.x.valueOf() + rect.width.valueOf(), rect.y),
      point(rect.x.valueOf() + rect.width.valueOf(), rect.y.valueOf() + rect.height.valueOf()),
      point(rect.x, rect.y.valueOf() + rect.height.valueOf())
   ])
}

function rect_stroke (rect: Rect): THREE.Object3D {
   return path_stroke(cons(point(rect.x, rect.y.valueOf() + rect.height.valueOf()), rect_path(rect)))
}

function rect_fill (rect: Rect): THREE.Object3D {
   const geometry: THREE.Geometry = newPathGeometry(rect_path(rect))
   geometry.faces.push(new THREE.Face3(0,1,2))
   geometry.faces.push(new THREE.Face3(2,3,0))
   return new THREE.Mesh(
      geometry, 
      new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
   )
}
