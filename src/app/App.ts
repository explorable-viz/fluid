import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import { __nonNull, as } from "../util/Core"
import { World } from "../util/Versioned"
import { Expr } from "../Expr"
import { Eval } from "../Eval"
import { GraphicsElement } from "../Graphics"
import { Value } from "../ExplVal"
// TODO: move test-dependent stuff out of app
import { Cursor } from "../../test/util/Cursor"
import { ρ, initialise, loadExample, parseExample } from "../../test/util/Core"
import { reflect } from "./Reflect"
import { Renderer } from "./Render"

initialise()

const scene = new THREE.Scene()
scene.background = new THREE.Color(0xffffff)
const camera = new THREE.PerspectiveCamera(
   /* field of view (degrees) */ 90,
   /* aspect ratio */            1,
   /* near */                    1,
   /* far */                     1000
)
camera.position.set(0, 0, 100)
camera.lookAt(new THREE.Vector3(0, 0, 0))

const renderer = new THREE.WebGLRenderer
renderer.setSize(1200, 1200)
renderer.setViewport(0, 0, 1200, 1200)

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

const canvas2d: HTMLCanvasElement = document.createElement("canvas")

document.body.appendChild(canvas2d)
document.body.appendChild(renderer.domElement)

export function close (path: THREE.Vector2[]) {
   return path.concat(path[0])
}

function populateScene (): void {
   const e: Expr = parseExample(loadExample("bar-chart"))
   World.newRevision()
   let here: Cursor = new Cursor(e)
   here
      .skipImports()
      .to(Expr.Let, "e")
   const data: Expr.Constr = as(here.o, Expr.Constr)
   here
      .constrArg("Cons", 0)
      .constrArg("Pair", 1)
      .constrArg("Cons", 0)
      .constrArg("Pair", 1)
      .constrArg("Cons", 0)
      .constrArg("Pair", 1).notNeed() // 2015 > China > Bio > [here]
   const v: Value = Eval.eval_(ρ, e).v,
         elem: GraphicsElement = as(reflect(v), GraphicsElement),
         renderer: Renderer = new Renderer()
   for (let obj of renderer.objects3D(elem)) {
      scene.add(obj)
   }
   scene.add(dataView(data))
}

function dataView (data: Expr.Constr): THREE.Object3D {
   const ctx: CanvasRenderingContext2D = __nonNull(canvas2d.getContext("2d"))

   ctx.font = "20pt Arial"
   ctx.fillStyle = "red"
   ctx.fillRect(0, 0, canvas2d.width, canvas2d.height)
   ctx.fillStyle = "white"
   ctx.fillRect(10, 10, canvas2d.width - 20, canvas2d.height - 20)
   ctx.fillStyle = "black"
   ctx.textAlign = "center"
   ctx.textBaseline = "middle"
   ctx.fillText(new Date().getTime().toString(), canvas2d.width / 2, canvas2d.height / 2)

   const texture = new THREE.Texture(canvas2d),
         material = new THREE.MeshBasicMaterial({ map: texture }),
         geometry = new THREE.BoxGeometry(200, 200, 200)
   return new THREE.Mesh(geometry, material)
}

function render () {
   renderer.render(scene, camera)
}

controls.addEventListener("change", render)
populateScene()
render()
