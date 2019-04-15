import * as THREE from "three"

export class GraphicsPane3D2 {
   camera: THREE.Camera
   scene: THREE.Scene
   renderer: THREE.WebGLRenderer
   geometry: THREE.Geometry
   texture: THREE.Texture
   mesh: THREE.Mesh

   constructor (width: number, height: number) {
      this.renderer = new THREE.WebGLRenderer
      this.renderer.setSize(width, height)
      this.scene = new THREE.Scene
      this.camera = new THREE.PerspectiveCamera(70, width / height, 1, 1000)
      this.camera.position.z = 500
      this.scene.add(this.camera)
   }
}
