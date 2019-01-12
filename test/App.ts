import * as THREE from "three"
import { __nonNull } from "../src/util/Core"
import { Traced } from "../src/Traced"
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

export function getPoints (tv: Traced): THREE.Vector3[] {
   console.log(tv.v)
   return []
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
