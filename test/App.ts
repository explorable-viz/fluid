import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"
import * as Meshline from "three.meshline"
import { __nonNull, assert } from "../src/util/Core"
import { Cons, List, Nil } from "../src/BaseTypes"
import { Traced, Value } from "../src/Traced"
import { TestFile, initialise, loadTestFile, runTest } from "../test/Helpers"

initialise()
const file: TestFile = loadTestFile("example", "bar-chart")
const [rects, axes]: [THREE.Vector2[][], THREE.Vector2[][]] = getRectsAxes(__nonNull(runTest(__nonNull(file.text))))

export function getRectsAxes (tv: Traced): [THREE.Vector2[][], THREE.Vector2[][]] {
   if (tv.v instanceof Value.Constr) {
      if (tv.v.ctr.str === "Pair") {
         const rects_axes: List<Traced> = tv.v.args
         if (Cons.is(rects_axes) && Cons.is(rects_axes.tail)) {
            return [getRects(rects_axes.head), getRects(rects_axes.tail.head)]
         }
      }
   }
   return assert(false)
}

class Rect_New {
   x: number
   y: number
   width: number
   height: number

   constructor (x: number, y: number, width: number, height: number) {
      this.x = x
      this.y = y
      this.width = width
      this.height = height
   }

   object3D (): THREE.Object3D {
      const geometry: THREE.Geometry = new THREE.Geometry
      geometry.vertices.push(new THREE.Vector3(this.x, this.y, 0))
      geometry.vertices.push(new THREE.Vector3(this.x + this.width, this.y, 0))
      geometry.vertices.push(new THREE.Vector3(this.x + this.width, this.y + this.height, 0))
      geometry.vertices.push(new THREE.Vector3(this.x, this.y + this.height, 0))
      geometry.faces.push(new THREE.Face3(0,1,2))
      geometry.faces.push(new THREE.Face3(2,3,0))
      return new THREE.Mesh(
         geometry, 
         new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
      )
   }
}

export function getRects_new (tv: Traced): Rect_New[] {
   if (tv.v instanceof Value.Constr) {
      if (tv.v.ctr.str === "Cons") {
         const rect_tvs: List<Traced> = tv.v.args
         if (Cons.is(rect_tvs) && Cons.is(rect_tvs.tail)) {
            return [getRect(rect_tvs.head)].concat(getRects_new(rect_tvs.tail.head))
         } 
      } else
      if (tv.v.ctr.str === "Nil" && Nil.is(tv.v.args)) {
         return []
      }
   }
   return assert(false)
}

function getRect (tv: Traced): Rect_New {
   if (tv.v instanceof Value.Constr && tv.v.ctr.str === "Rect") {
      const tvs: List<Traced> = tv.v.args
      if (Cons.is(tvs) && tvs.head instanceof Value.ConstInt && 
          Cons.is(tvs.tail) && tvs.tail.head instanceof Value.ConstInt &&
          Cons.is(tvs.tail.tail) && tvs.tail.tail.head instanceof Value.ConstInt && 
          Cons.is(tvs.tail.tail.tail) && tvs.tail.tail.tail.head instanceof Value.ConstInt) {
         return new Rect_New(
            tvs.head.val,
            tvs.tail.head.val,
            tvs.tail.tail.head.val,
            tvs.tail.tail.tail.head.val
         )
      } 
   }
   return assert(false)
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

export class Rect extends THREE.Geometry {
   constructor (rect: THREE.Vector2[]) {
      super()
      for (const point of rect) {
         this.vertices.push(new THREE.Vector3(point.x, point.y, 0))
      }
      this.faces.push(new THREE.Face3(0,1,2))
      this.faces.push(new THREE.Face3(2,3,0))
   }

   object3D (): THREE.Object3D {
      return new THREE.Mesh(
         this, 
         new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
      )
   }
}

export class Path extends THREE.Geometry {
   constructor (path: THREE.Vector2[]) {
      super()   
      for (const point of path) {
         this.vertices.push(new THREE.Vector3(point.x, point.y, 0))
      }
   }

    object3D (): THREE.Object3D {
      return new THREE.Line(this, new THREE.LineBasicMaterial({ 
         color: 0x000000 
      }))
   }
}

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

function close (path: THREE.Vector2[]) {
   return path.concat(path[0])
}

function populateScene (): void {
   for (let rect of rects) {
      assert(rect.length === 4)
      scene.add(new Rect(rect).object3D())
      scene.add(new Path(close(rect)).object3D())
   }
   for (let line of axes) {
      assert(line.length === 2)
      scene.add(new Path(line).object3D())
   }
}

function render () {
   renderer.render(scene, camera)
}

controls.addEventListener("change", render)
populateScene()
render()
