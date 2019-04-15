import * as THREE from "three"

export class GraphicsPane3D {
   scene: THREE.Scene
   renderer: THREE.WebGLRenderer
   camera: THREE.Camera
   
   constructor () {
      this.scene = new THREE.Scene
      this.renderer = new THREE.WebGLRenderer
      this.camera = new THREE.PerspectiveCamera(
         /* field of view (degrees) */ 90,
         /* aspect ratio */            1,
         /* near */                    1,
         /* far */                     1000
      )
   }

   render () {
      this.renderer.render(this.scene, this.camera)
   }
}