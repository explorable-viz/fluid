import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import { as } from "../util/Core"
import { World } from "../util/Versioned"
import { Cons, List } from "../BaseTypes"
import { Expr } from "../Expr"
import { Eval } from "../Eval"
import { GraphicsElement, objects3D } from "../Graphics"
import { Value } from "../ExplVal"
// TODO: move test-dependent stuff out of app
import { Cursor } from "../../test/util/Cursor"
import { ρ, initialise, loadTestFile, parseExample } from "../../test/util/Core"
import { reflect } from "./Reflect"

initialise()

const scene = new THREE.Scene()
scene.background = new THREE.Color( 0xffffff )
const camera = new THREE.PerspectiveCamera( 60, 1, 1, 200 )
camera.position.set( 0, 0, 100 )
camera.lookAt( new THREE.Vector3(0, 0, 0) )

const renderer = new THREE.WebGLRenderer
renderer.setSize( 600, 600 )

const controls = new OrbitControls( camera, renderer.domElement );

// How far you can orbit vertically, upper and lower limits.
controls.minPolarAngle = 0;
controls.maxPolarAngle = Math.PI;

// How far you can dolly in and out ( PerspectiveCamera only )
controls.minDistance = 0;
controls.maxDistance = Infinity;

controls.enableZoom = true; // Set to false to disable zooming
controls.zoomSpeed = 1.0;

controls.enablePan = true; // Set to false to disable panning (ie vertical and horizontal translations)

controls.enableDamping = true; // Set to false to disable damping (ie inertia)
controls.dampingFactor = 0.25;

document.body.appendChild(renderer.domElement)

export function close (path: THREE.Vector2[]) {
   return path.concat(path[0])
}

function populateScene (): void {
   const e: Expr = parseExample(loadTestFile("example", "bar-chart").text)
   World.newRevision()
   let here: Cursor = new Cursor(e)
   here
      .to(Expr.Let, "e")
      .constrArg("Cons", 0).notNeed()
      .constrArg("Pair", 1).notNeed()
   const v: Value = Eval.eval_(ρ, e).v,
         elems: List<GraphicsElement> = as(reflect(v), List)
   for (let elemsʹ: List<GraphicsElement> = elems; Cons.is(elemsʹ);) {
      for (let obj of objects3D(as(elemsʹ.head, GraphicsElement))) {
         scene.add(obj)
      }
      elemsʹ = elemsʹ.tail
   }
}

function render () {
   renderer.render(scene, camera)
}

controls.addEventListener("change", render)
populateScene()
render()
