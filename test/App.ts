import * as THREE from "three"
import { __nonNull } from "../src/util/Core"
import { Eval } from "../src/Eval"
import { instantiate } from "../src/Instantiate"
import { Profile, TestFile, ρ, τ, initialise, loadTestFile, runTest } from "../test/Helpers"

initialise()
const file: TestFile = loadTestFile("example", "factorial")
runTest(__nonNull(file.text), Profile.Match, τ.var_(null))

function getPoints (n: number): THREE.Vector3[] {
   const [tv, , ]: Eval.Result<null> = Eval.eval_(ρ, instantiate(ρ)(e), σ)
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
