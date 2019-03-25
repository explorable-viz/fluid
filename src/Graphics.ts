import * as THREE from "three"
import { absurd, as } from "./util/Core"
import { Persistent, PersistentObject, make } from "./util/Persistent"
import { Cons, List, cons } from "./BaseTypes"

// Basic graphical datatypes.

export class Rect implements PersistentObject {
   x: number
   y: number
   width: number
   height: number

   constructor_ (x: number, y: number, width: number, height: number) {
      this.x = x
      this.y = y
      this.width = width
      this.height = height
   }

   static make (x: number, y: number, width: number, height: number): Rect {
      return make(Rect, x, y, width, height)
   }
}

export class Point implements PersistentObject {
   x: number
   y: number

   constructor_ (x: number, y: number) {
      this.x = x
      this.y = y
   }

   static make (x: number, y: number): Point {
      return make(Point, x, y)
   }
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
      geometry.vertices.push(new THREE.Vector3(point.x, point.y, 0))
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
      Point.make(rect.x, rect.y),
      Point.make(rect.x + rect.width, rect.y),
      Point.make(rect.x + rect.width, rect.y + rect.height),
      Point.make(rect.x, rect.y + rect.height)
   ])
}

function rect_stroke (rect: Rect): THREE.Object3D {
   return path_stroke(cons(Point.make(rect.x, rect.y + rect.height), rect_path(rect)))
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
