import * as THREE from "three"
import { __nonNull, assert } from "../src/util/Core"
import { Cons, List, Nil } from "../src/BaseTypes"
import { Traced, Value } from "../src/Traced"
import { Profile, TestFile, τ, initialise, loadTestFile, runTest } from "../test/Helpers"

import Trie = Traced.Trie

initialise()
const file: TestFile = loadTestFile("example", "zipW")
getPoints(__nonNull(runTest(__nonNull(file.text), Profile.Match, points(3, null))))

// Demand for list of points of length n.
export function points<K> (n: number, κ: K): Trie.Constr<K> {
   if (n === 0) {
      return τ.nil(τ.endArgs(κ))
   } else {
      return τ.cons(τ.arg(τ.point(τ.arg(τ.int(τ.arg(τ.int(τ.endArgs(τ.arg(points(n - 1, τ.endArgs(κ)))))))))))
   }
}

// Hack to suck out the leaf data. Might have to rethink what it means to match primitive data.
export function getPoints (tv: Traced): THREE.Vector3[] {
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
                        return [new THREE.Vector3(x_y.head.v.val, x_y.tail.head.v.val)]
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
const camera = new THREE.PerspectiveCamera( 45, 1, 1, 500 )
camera.position.set( 0, 0, 100 )
camera.lookAt( new THREE.Vector3(0, 0, 0) )

const renderer = new THREE.WebGLRenderer()
renderer.setSize( 600, 600 )
document.body.appendChild( renderer.domElement )

const geometry = new THREE.Geometry()
geometry.vertices.push(new THREE.Vector3( -10, 0, 0) )
geometry.vertices.push(new THREE.Vector3( 0, 10, 0) )
geometry.vertices.push(new THREE.Vector3( 10, 0, 0) )
const material = new THREE.LineBasicMaterial( { color: 0x0000ff } )
scene.add(new THREE.Line( geometry, material ))

renderer.render( scene, camera )
