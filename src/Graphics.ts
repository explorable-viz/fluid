import * as THREE from "three"
import { absurd, make } from "./util/Core"
import { Cons, List } from "../src/BaseTypes"
import { InternedObject } from "./Runtime"

// Basic graphical datatypes.

export class Rect {
   x: number
   y: number
   width: number
   height: number

   constructor (x: number, y: number, width: number, height: number) {
      this.x = x
      this.y = y
      this.width = width
      this.height = height
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
export function object3D (elem: Object): THREE.Object3D {
   if (elem instanceof Rect) {
      return rect_object3D(elem)
   } else 
   if (elem instanceof List) { // ouch
      return path_object3D(elem)
   } else {
      return absurd()
   }
}

function rect_object3D (rect: Rect): THREE.Object3D {
   const geometry: THREE.Geometry = new THREE.Geometry
   geometry.vertices.push(new THREE.Vector3(rect.x, rect.y, 0))
   geometry.vertices.push(new THREE.Vector3(rect.x + rect.width, rect.y, 0))
   geometry.vertices.push(new THREE.Vector3(rect.x + rect.width, rect.y + rect.height, 0))
   geometry.vertices.push(new THREE.Vector3(rect.x, rect.y + rect.height, 0))
   geometry.faces.push(new THREE.Face3(0,1,2))
   geometry.faces.push(new THREE.Face3(2,3,0))
   return new THREE.Mesh(
      geometry, 
      new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
   )
}

function path_object3D (points: List<Point>): THREE.Object3D {
   const geometry: THREE.Geometry = new THREE.Geometry
   while (Cons.is(points)) {
      geometry.vertices.push(new THREE.Vector3(points.head.x, points.head.y, 0))
      points = points.tail
   }
   return new THREE.Line(geometry, new THREE.LineBasicMaterial({ 
      color: 0x000000 
   }))
}

