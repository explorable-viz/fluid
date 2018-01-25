import * as THREE from "three"
import { True } from "./BaseTypes"
import { __nonNull } from "./Util"
import {
  Background, EmptyView, Horiz, RoundedCell, Space, Vert, View, ViewVisitor, Word
} from "./View"

export module Render {

export function renderInDoc (w: View): void {
   document.body.appendChild(domElement(__nonNull(new Renderer().render(w))))
}

var renderer: THREE.WebGLRenderer,
    camera: THREE.Camera,
    scene: THREE.Scene,
    controls: THREE.TrackballControls

function domElement (root: THREE.Object3D): HTMLElement {
   renderer = new THREE.WebGLRenderer( { precision: "highp" })
   var x: number = window.innerWidth,
       y: number = window.innerHeight
   renderer.setSize(x, y)
   camera = false ? new THREE.OrthographicCamera(-x / 2, x / 2, y / 2, -y / 2, 1, 10000)
                  : new THREE.PerspectiveCamera(30,x / y, 1, 10000)
   camera.position.z = 1000

   scene = new THREE.Scene()
   scene.add(root)
   render()

   controls = new THREE.TrackballControls(camera/*, document*/)
   controls.rotateSpeed = 1.0
   controls.zoomSpeed = 1.2
   controls.panSpeed = 0.8
   controls.noZoom = false
   controls.noPan = false
   controls.staticMoving = true
   controls.dynamicDampingFactor = 0.3
   controls.keys = [65, 83, 68]
   controls.addEventListener('change', render)

   animate()
   return renderer.domElement
}

function animate () {
   requestAnimationFrame(animate)
   controls.update()
}

// TODO: 'render' is a little bit overloaded here ;-)
function render () {
   renderer.render(scene, camera)
}

// Note that "null" does not represent ⊥ (as elsewhere), but the absence of an Object3D.
// (⊥ will be rendered, and so associated with an Object3D.) See 0.4.5, 0.5 and 0.5.1
// release notes.
class Renderer extends ViewVisitor<THREE.Object3D | null> {
   render (w: View): THREE.Object3D | null {
      return w.__visit<THREE.Object3D | null>(this)
   }

   size (object: THREE.Object3D): THREE.Vector3 {
      return new THREE.Box3().setFromObject(object).getSize()
   }

   is_EmptyView (w: EmptyView): null {
      return null
   }

   is_Space (w: Space): THREE.Object3D {
      return createText(" ", false, greyScaleToRGB(0))
   }

   is_Background (w: Background): THREE.Object3D | null {
      var child: THREE.Object3D | null = this.render(w.child)
      if (child === null)
         return null
      else {
         var size: THREE.Vector3 = this.size(child),
             behind: THREE.Mesh = new THREE.Mesh(
                new THREE.PlaneGeometry(size.x, size.y),
                new THREE.MeshBasicMaterial({ color: greyScaleToRGB(w.greyScale.val)/*, overdraw: false*/ })
             ),
             background: THREE.Object3D = new THREE.Object3D()
         child.position.z = 1
         behind.position.x = size.x / 2
         behind.position.y = -size.y / 2
         background.add(behind)
         background.add(child)
         return background
      }
   }

   // My child must not be empty. For now, don't do anything with rounded cells anyway.
   is_RoundedCell (w: RoundedCell): THREE.Object3D {
      return __nonNull(this.render(w.child))
   }

   is_Word (w: Word): THREE.Object3D {
      return createTextFromWord(w)
   }

   is_Horiz (w: Horiz): THREE.Object3D | null {
      var child1: THREE.Object3D | null = this.render(w.child1),
          child2: THREE.Object3D | null = this.render(w.child2)
      if (child1 === null)
         return child2
      if (child2 === null)
         return child1
      child2.position.x += this.size(child1).x
      var horiz: THREE.Object3D = new THREE.Object3D()
      horiz.add(child1)
      horiz.add(child2)
      return horiz
   }

   is_Vert (w: Vert): THREE.Object3D | null {
      var child1: THREE.Object3D | null = this.render(w.child1),
          child2: THREE.Object3D | null = this.render(w.child2)
      if (child1 === null)
         return child2
      if (child2 === null)
         return child1
      child2.position.y -= this.size(child1).y
      var vert: THREE.Object3D = new THREE.Object3D()
      vert.add(child1)
      vert.add(child2)
      return vert
   }
}

function createText (str: string, bold: boolean, color: number): THREE.Mesh {
   var canvas: HTMLCanvasElement = <HTMLCanvasElement>document.createElement("canvas"),
       context = __nonNull(canvas.getContext("2d")),
       size = 20
   context.font = size + "pt ubuntu mono"
   var textWidth = context.measureText(str).width

   canvas.width = textWidth
   canvas.height = size * 1.5 // TODO: fix a row height
   // Don't know why we have to do this twice.
   context = __nonNull(canvas.getContext("2d"))
   context.font = size + "pt ubuntu mono"

   context.textAlign = "left"
   context.textBaseline = "top"
   context.fillStyle = "#000000"
   context.fillText(str, 0, 0)

   var texture = new THREE.Texture(canvas)
   texture.needsUpdate = true
   var text: THREE.Mesh = new THREE.Mesh(
      new THREE.PlaneGeometry(canvas.width, canvas.height),
      new THREE.MeshBasicMaterial({ map : texture, transparent: true })
   )
   text.position.x = canvas.width / 2
   text.position.y = -canvas.height / 2
   return text
}

function createTextFromWord (w: Word): THREE.Mesh {
   return createText(w.str.val, w.bold instanceof True, greyScaleToRGB(w.greyScale.val))
}

// greyScale is in the range 0..255
function greyScaleToRGB (greyScale: number): number {
   return greyScale + greyScale * Math.pow(16, 2) + greyScale * Math.pow(16, 4)
}

}
