import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import { Class, __check, __nonNull, as, absurd } from "../src/util/Core"
import { diffProp } from "../src/util/Delta"
import { Persistent, PersistentObject, VersionedObject, World, at, make, versioned, __w } from "../src/util/Persistent"
import { Cons, List, Nil } from "../src/BaseTypes"
import { arity } from "../src/DataType"
import { Expr } from "../src/Expr"
import { Point, Rect, objects } from "../src/Graphics"
import { Traced, Value } from "../src/Traced"
import { initialise, loadTestFile, runExample, parseExample } from "../test/Helpers"

initialise()

// intermediate value required to stop TS getting confused:
const classFor_: [string, Class<PersistentObject>][] =
   [["Cons", Cons],
    ["Nil", Nil],
    ["Point", Point],
    ["Rect", Rect]]
const classFor: Map<string, Class<PersistentObject>> = new Map(classFor_)

// Not really convinced by this pattern - wouldn't it make more sense to use the function objects themselves
// to partition the memo keys, as I did in lambdacalc-old?
class Reflect implements PersistentObject {
   v: Value

   constructor_ (v: Value) {
      this.v = v
   }

   static make (v: Value): Reflect {
      return make<Reflect>(Reflect, v)
   }
}

function reflect (v: Value | null): Persistent { // weirdly number and string are subtypes of Object
   if (v === null) {
      return null
   } else
   if (v instanceof Value.ConstInt) {
      return v.val
   } else
   if (v instanceof Value.ConstStr) {
      return v.val
   } else
   if (v instanceof Value.Constr) {
      const ctr: string = __check(v.ctr.str, it => classFor.has(it)),
            args: Persistent[] = []
      for (let tvs: List<Traced> = v.args; Cons.is(tvs);) {
         args.push(reflect(tvs.head.v))
         tvs = tvs.tail
      }
      return at(Reflect.make(v), classFor.get(ctr)!, ...__check(args, it => it.length === arity(ctr)))
   } else {
      return absurd()
   }
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

export function close (path: THREE.Vector2[]) {
   return path.concat(path[0])
}

function from<T extends PersistentObject> (o: PersistentObject, cls: Class<T>, prop: keyof T): Persistent {
   return as<PersistentObject, T>(o, cls)[prop] as Object as Persistent
}

function populateScene (): void {
   const e: Expr.Expr = parseExample(loadTestFile("example", "bar-chart").text),
         v: Value.Value = __nonNull(runExample(e).v),
         elems: List<Persistent> = as(reflect(v), List),
         w: World = __w
   if (versioned(e)) {
      let here: Persistent = e
      here = from(here as PersistentObject, Expr.Let, "e")
      here = from(here as PersistentObject, Expr.Constr, "args")
      here = from(here as PersistentObject, Cons, "head")
      World.newRevision()
      
      // TODO: make some change at __w and reevaluate
      for (let elemsʹ: List<Persistent> = elems; Cons.is(elemsʹ);) {
         // assume only increasing or decreasing changes (to or from null):
         diffProp(elemsʹ, "head", w)
         for (let obj of objects(elemsʹ.head)) {
            scene.add(obj)
         }
         elemsʹ = elemsʹ.tail
      }
   } else {
      absurd()
   }
}

function render () {
   renderer.render(scene, camera)
}

controls.addEventListener("change", render)
populateScene()
render()
