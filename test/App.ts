import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import * as Meshline from "three.meshline"
import { Class, Persistent, PersistentObject, __check, __nonNull, assert, absurd, make } from "../src/util/Core"
import { Cons, List, Nil } from "../src/BaseTypes"
import { arity } from "../src/DataType"
import { Traced, Value } from "../src/Traced"
import { Point, Rect, objects } from "../src/Graphics"
import { TestFile, initialise, loadTestFile, runTest } from "../test/Helpers"

initialise()
const file: TestFile = loadTestFile("example", "bar-chart")

// intermediate value required to stop TS getting confused:
const classFor_: [string, Class<PersistentObject>][] =
   [["Cons", Cons],
    ["Nil", Nil],
    ["Point", Point],
    ["Rect", Rect]]
const classFor: Map<string, Class<PersistentObject>> = new Map(classFor_)

function reflect (v: Value | null): Persistent { // weirdy number and string are subtypes of Object
   if (v instanceof Value.ConstInt) {
      return v.val
   } else
   if (v instanceof Value.ConstStr) {
      return v.val
   } else
   if (v instanceof Value.Constr) {
      const ctr: string = v.ctr.str
      assert(classFor.has(ctr))
      const args: Persistent[] = []
      for (let tvs: List<Traced> = v.args; Cons.is(tvs);) {
         args.push(reflect(tvs.head.v))
         tvs = tvs.tail
      }
      // interning probably not what we want here, but for now will do
      return make(classFor.get(ctr)!, ...__check(args, it => it.length === arity(ctr)))
   } else {
      return absurd()
   }
}

// List at the outer level assumed to be a collection of graphics elements. List one level down
// assumed to be a path (list of points).
function getElems (tv: Traced): List<Persistent> {
   if (tv.v instanceof Value.Constr) {
      if (tv.v.ctr.str === "Cons") {
         const rect_tvs: List<Traced> = tv.v.args
         if (Cons.is(rect_tvs) && Cons.is(rect_tvs.tail)) {
            return Cons.make(reflect(__nonNull(rect_tvs.head.v)), getElems(rect_tvs.tail.head))
         } 
      } else
      if (tv.v.ctr.str === "Nil" && Nil.is(tv.v.args)) {
         return Nil.make()
      }
   }
   return assert(false)
}

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

// Currently unused.
export class ThickPath extends THREE.Geometry {
   constructor (path: THREE.Vector2[]) {
      super()   
      for (const point of path) {
         this.vertices.push(new THREE.Vector3(point.x, point.y, 0))
      }
   }

   object3D (): THREE.Object3D {
      const line = new Meshline.MeshLine()
      line.setGeometry(this)
      const material = new Meshline.MeshLineMaterial({
         color: new THREE.Color(0x000000),
         sizeAttenuation: 0,
         lineWidth: 0.020
      })
      return new THREE.Mesh( line.geometry, material )
   }
}

export function close (path: THREE.Vector2[]) {
   return path.concat(path[0])
}

function populateScene (): void {
   for (let elems: List<Persistent> = getElems(__nonNull(runTest(__nonNull(file.text)))); Cons.is(elems);) {
      for (let obj of objects(elems.head)) {
         scene.add(obj)
      }
      elems = elems.tail
   }
}

function render () {
   renderer.render(scene, camera)
}

controls.addEventListener("change", render)
populateScene()
render()
