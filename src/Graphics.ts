import * as THREE from "three"
import { absurd, assert, make } from "./util/Core"
import { Cons, List, Nil } from "../src/BaseTypes"
import { InternedObject } from "./Runtime"

// Basic graphical datatypes.

export class Rect extends InternedObject {
   x: number
   y: number
   width: number
   height: number

   constructor (x: number, y: number, width: number, height: number) {
      super()
      this.x = x
      this.y = y
      this.width = width
      this.height = height
   }

   static make (x: number, y: number, width: number, height: number): Rect {
      return make(Rect, x, y, width, height)
   }
}

export class Point extends InternedObject { // for now
   x: number
   y: number

   constructor (x: number, y: number) {
      super()
      this.x = x
      this.y = y
   }

   static make (x: number, y: number): Point {
      return make(Point, x, y)
   }
}

// We don't have anything like typeclasses yet.
export function objects (elem: Object): THREE.Object3D[] {
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
      assert(points.head instanceof Point)
      geometry.vertices.push(new THREE.Vector3(points.head.x, points.head.y, 0))
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
   return Cons.make(
      Point.make(rect.x, rect.y),
      Cons.make(
         Point.make(rect.x + rect.width, rect.y),
         Cons.make(
            Point.make(rect.x + rect.width, rect.y + rect.height),
            Cons.make(
               Point.make(rect.x, rect.y + rect.height),
               Nil.make()
            )
         )
      )
   )
}

function rect_stroke (rect: Rect): THREE.Object3D {
   return path_stroke(rect_path(rect))
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
