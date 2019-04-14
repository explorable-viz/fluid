import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import { __nonNull, absurd, as } from "../util/Core"
import { PersistentObject} from "../util/Persistent"
import { World } from "../util/Versioned"
import { Cons, List, Nil, Pair } from "../BaseTypes"
import { Expr } from "../Expr"
import { Eval } from "../Eval"
import { AnnNumber, AnnString, GraphicsElement } from "../Graphics"
import { Value } from "../ExplVal"
// TODO: move test-dependent stuff out of app
import { Cursor } from "../../test/util/Cursor"
import { ρ, initialise, load, parse } from "../../test/util/Core"
import { reflect } from "./Reflect"
import { Renderer, to3DTextureMap } from "./Render"

initialise()

const scene = new THREE.Scene(),
      dataCanvas: HTMLCanvasElement = document.createElement("canvas"),
      viewCanvas: HTMLCanvasElement = document.createElement("canvas"),
      renderer = new THREE.WebGLRenderer,
      camera = new THREE.PerspectiveCamera(
         /* field of view (degrees) */ 90,
         /* aspect ratio */            1,
         /* near */                    1,
         /* far */                     1000
      )
   
function initialiseScene (): void {
   scene.background = new THREE.Color(0xffffff)
   camera.position.set(0, 0, 75)
   camera.lookAt(new THREE.Vector3(0, 0, 0))
   
   renderer.setSize(800, 800)
   renderer.setViewport(0, 0, 800, 800)
   
   const controls = new OrbitControls(camera, renderer.domElement)
   
   // How far you can orbit vertically, upper and lower limits.
   controls.minPolarAngle = 0
   controls.maxPolarAngle = Math.PI
   
   // How far you can dolly in and out ( PerspectiveCamera only )
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
   viewCanvas.style.verticalAlign = "top"
   viewCanvas.style.display = "inline-block"
   renderer.domElement.style.display = "inline-block"
   document.body.appendChild(dataCanvas)
   document.body.appendChild(viewCanvas)
   document.body.appendChild(renderer.domElement)
}

export function close (path: THREE.Vector2[]) {
   return path.concat(path[0])
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
         renderer: Renderer = new Renderer(__nonNull(dataCanvas.getContext("2d")))
   for (let obj of renderer.objects3D(elem)) {
      scene.add(obj)
   }
   scene.add(renderer.render(elem))
   // TODO: when backward slicing, will have to "re-get" the state of data to pick up the slicing information; not nice.
   const dataRenderer = new DataRenderer(__nonNull(dataCanvas.getContext("2d"))),
         dataʹ: Data = as(reflect(data), List)
   dataRenderer.render(dataʹ) // draw once to compute size
   dataCanvas.height = (dataRenderer.lines) * dataRenderer.lineHeight
   scene.add(dataRenderer.render(dataʹ)) // draw again
}

type Data = List<Pair<AnnNumber | AnnString, PersistentObject>> // approximate recursive type

class DataRenderer {
   ctx: CanvasRenderingContext2D
   lineHeight: number
   lines: number

   constructor (cxt: CanvasRenderingContext2D) {
      this.ctx = cxt
      this.ctx.font = "10pt Arial"
      this.ctx.textAlign = "left"
      this.ctx.textBaseline = "middle"
      // No easy way to access text height, but this will do for now.
      // https://stackoverflow.com/questions/1134586
      this.lineHeight = this.ctx.measureText("M").width
   }

   render (data: Data): THREE.Object3D {
      this.lines = 0
      this.renderData(0, data)
      return to3DTextureMap(this.ctx.canvas)
   }

   renderData (indentx: number, data: Data): void {
      if (Cons.is(data)) {
         ++this.lines
         const { fst: key, snd: val }: Pair<AnnNumber | AnnString, PersistentObject> = as(data.head, Pair)
         let keyStr: string
         this.ctx.fillStyle = key.α ? "black" : "red"
         if (key instanceof AnnNumber) {
            keyStr = key.n.toString()
         } else
         if (key instanceof AnnString) {
            keyStr = key.str
         } else {
            return absurd()
         }
         keyStr += ": "
         this.ctx.fillText(keyStr, indentx, this.lines * this.lineHeight)
         const newIndentx = indentx + this.ctx.measureText(keyStr).width
         let valStr: string
         if (val instanceof List) {
            this.renderData(newIndentx, val as Data)
         } else 
         if (val instanceof AnnNumber || val instanceof AnnString) {
            this.ctx.fillStyle = val.α ? "black" : "red"
            if (val instanceof AnnNumber) {
               valStr = val.n.toString()
            } else {
               valStr = val.str
            }
            this.ctx.fillText(valStr, newIndentx, this.lines * this.lineHeight)
         } else {
            return absurd()
         }
         this.renderData(indentx, data.tail)
      } else
      if (Nil.is(data)) {
         return
      } else {
         return absurd()
      }
   }
}

function render () {
   renderer.render(scene, camera)
}

initialiseScene()
populateScene()
render()
