import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import { __nonNull, as } from "../util/Core"
import { World } from "../util/Versioned"
import { List } from "../BaseTypes"
import { Expr } from "../Expr"
import { Eval } from "../Eval"
import { GraphicsElement } from "../Graphics"
import { Value } from "../ExplVal"
// TODO: move test-dependent stuff out of app
import { Cursor } from "../../test/util/Cursor"
import { ρ, initialise, load, parse } from "../../test/util/Core"
import { Data, DataRenderer } from "./DataRenderer"
import { GraphicsPane3D } from "./GraphicsPane3D"
import { GraphicsRenderer } from "./GraphicsRenderer"
import { reflect } from "./Reflect"

const graphicsPane3D = new GraphicsPane3D(),
      dataCanvas: HTMLCanvasElement = document.createElement("canvas"),
      graphCanvas: HTMLCanvasElement = document.createElement("canvas"),
      renderer = new THREE.WebGLRenderer,
      camera = new THREE.PerspectiveCamera(
         /* field of view (degrees) */ 90,
         /* aspect ratio */            1,
         /* near */                    1,
         /* far */                     1000
      )
   
initialise()
initialiseScene()
populateScene()
render()

function initialiseScene (): void {
   graphicsPane3D.scene.background = new THREE.Color(0xffffff)
   camera.position.set(0, 0, 75)
   camera.lookAt(new THREE.Vector3(0, 0, 0))
   
   const controls = new OrbitControls(camera, renderer.domElement)
   
   // How far you can orbit vertically, upper and lower limits.
   controls.minPolarAngle = 0
   controls.maxPolarAngle = Math.PI
   
   // How far you can dolly in and out (PerspectiveCamera only)
   controls.minDistance = 0
   controls.maxDistance = Infinity
   
   controls.enableZoom = true // Set to false to disable zooming
   controls.zoomSpeed = 1.0
   
   controls.enablePan = true // Set to false to disable panning (ie vertical and horizontal translations)
   
   controls.enableDamping = true // Set to false to disable damping (ie inertia)
   controls.dampingFactor = 0.25
   controls.addEventListener("change", render)
   
   dataCanvas.style.verticalAlign = "top"
   dataCanvas.style.display = "inline-block"
   graphCanvas.height = 600
   graphCanvas.width = 600
   graphCanvas.style.verticalAlign = "top"
   graphCanvas.style.display = "inline-block"
   renderer.setSize(800, 800)
   renderer.setViewport(0, 0, 800, 800)
   renderer.domElement.style.display = "inline-block"
   document.body.appendChild(dataCanvas)
   document.body.appendChild(graphCanvas)
   document.body.appendChild(renderer.domElement)
}

function populateScene (): void {
   const e: Expr = parse(load("bar-chart"))
   World.newRevision()
   let here: Cursor = new Cursor(e)
   here
      .skipImports()
      .to(Expr.Let, "e")
      .constrArg("Cons", 0)
      .constrArg("Pair", 1)
      .constrArg("Cons", 0)
      .constrArg("Pair", 1)
      .constrArg("Cons", 0)
      .constrArg("Pair", 1).notNeed() // 2015 > China > Bio > [here]
   here = new Cursor(e)
      .skipImports()
      .to(Expr.Let, "e")
   const data: Value.Constr = as(Eval.eval_(ρ, as(here.o, Expr.Constr)).v, Value.Constr), // eval just to get a handle on it
         v: Value = Eval.eval_(ρ, e).v,
         elem: GraphicsElement = as(reflect(v), GraphicsElement),
         graphRenderer: GraphicsRenderer = new GraphicsRenderer(__nonNull(graphCanvas.getContext("2d")))
   graphRenderer.render(elem)
   graphicsPane3D.scene.add(to3DTextureMap(graphCanvas))
   // TODO: when backward slicing, will have to "re-get" the state of data to pick up the slicing information; not nice.
   const dataRenderer = new DataRenderer(__nonNull(dataCanvas.getContext("2d"))),
         dataʹ: Data = as(reflect(data), List)
   dataRenderer.render(dataʹ) // draw once to compute size
   dataCanvas.height = (dataRenderer.lines) * dataRenderer.lineHeight
   dataRenderer.render(dataʹ) // draw again
}

function to3DTextureMap (canvas: HTMLCanvasElement): THREE.Object3D {
   const texture = new THREE.Texture(canvas)
   texture.needsUpdate = true
   const material = new THREE.MeshBasicMaterial({ map: texture }),
         geometry = new THREE.BoxGeometry(200, 200, 200)
   return new THREE.Mesh(geometry, material)
}

function render () {
   renderer.render(graphicsPane3D.scene, camera)
}
