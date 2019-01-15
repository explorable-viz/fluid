import * as THREE from "three"
import { __nonNull, assert } from "../src/util/Core"
import { Cons, List, Nil } from "../src/BaseTypes"
import { Traced, Value } from "../src/Traced"
import { Profile, TestFile, τ, initialise, loadTestFile, runTest } from "../test/Helpers"

import Trie = Traced.Trie

initialise()
const file: TestFile = loadTestFile("example", "bar-chart")
const rects: THREE.Vector2[][] = getRects(__nonNull(runTest(__nonNull(file.text), Profile.Match, expectRects(4, null))))

// Demand for list of points of length n.
export function expectPoints<K> (n: number, κ: K): Trie.Constr<K> {
   if (n === 0) {
      return τ.nil(τ.endArgs(κ))
   } else {
      return τ.cons(τ.arg(τ.point(τ.arg(τ.int(τ.arg(τ.int(τ.endArgs(τ.arg(expectPoints(n - 1, τ.endArgs(κ)))))))))))
   }
}

export function expectRects<K> (n: number, κ: K): Trie.Constr<K> {
   if (n === 0) {
      return τ.nil(τ.endArgs(κ))
   } else {
      return τ.cons(τ.arg(expectPoints(4, τ.arg(expectRects(n - 1, τ.endArgs(κ))))))
   }
}

// Hack to suck out the leaf data. Might have to rethink what it means to match primitive data.
export function getRects (tv: Traced): THREE.Vector2[][] {
   if (tv.v instanceof Value.Constr) {
      if (tv.v.ctr.str === "Cons") {
         const points_tvs: List<Traced> = tv.v.args
         if (Cons.is(points_tvs) && Cons.is(points_tvs.tail)) {
            return [getPoints(points_tvs.head)].concat(getRects(points_tvs.tail.head))
         } 
      } else
      if (tv.v.ctr.str === "Nil" && Nil.is(tv.v.args)) {
         return []
      }
   }
   return assert(false)
}

export function getPoints (tv: Traced): THREE.Vector2[] {
   if (tv.v instanceof Value.Constr) {
      if (tv.v.ctr.str === "Cons") {
         const point_tvs: List<Traced> = tv.v.args
         if (Cons.is(point_tvs)) {
            const point: Traced = point_tvs.head
            if (point.v instanceof Value.Constr && point.v.ctr.str === "Point") {
               const x_y: List<Traced> = point.v.args
               if (Cons.is(x_y)) {
                  if (x_y.head.v instanceof Value.ConstInt && Cons.is(x_y.tail) &&
                     x_y.tail.head.v instanceof Value.ConstInt && Cons.is(point_tvs.tail)) {
                        return [new THREE.Vector2(x_y.head.v.val, x_y.tail.head.v.val)]
                                 .concat(getPoints(point_tvs.tail.head))
                  }
               }
            }
         } 
      } else
      if (tv.v.ctr.str === "Nil" && Nil.is(tv.v.args)) {
         return []
      }
   }
   return assert(false)
}

const scene = new THREE.Scene()
scene.background = new THREE.Color( 0xffffff )
const camera = new THREE.PerspectiveCamera( 50, 1, 1, 500 )
camera.position.set( 0, 0, 100 )
camera.lookAt( new THREE.Vector3(0, 0, 0) )

const renderer = new THREE.WebGLRenderer
renderer.setSize( 600, 600 )
document.body.appendChild( renderer.domElement )

const geometry = new THREE.Geometry()
for (const point of rects[0].slice(1)) {
   geometry.vertices.push(new THREE.Vector3(point.x, point.y, 0))
}
  
geometry.faces.push(new THREE.Face3(0,1,2))
geometry.faces.push(new THREE.Face3(1,2,3))

const material = new THREE.MeshBasicMaterial( { color: 0xF6831E, side: THREE.DoubleSide } );
const square_mesh = new THREE.Mesh(geometry, material)
scene.add(square_mesh)

// const poly = new THREE.Shape
// poly.moveTo(points[0].x, points[0].y)
// for (const point of points.slice(1)) {
//     poly.lineTo(point.x, point.y)
// }
// poly.lineTo(points[0].x, points[0].y)
// const geometry = new THREE.ShapeGeometry(poly)
// const material = new THREE.LineBasicMaterial( { color: 0x0000ff } )
// scene.add(new THREE.Line( geometry, material ))

renderer.render( scene, camera )
