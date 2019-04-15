import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"

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
      this.scene.background = new THREE.Color(0xffffff)
      this.camera.position.set(0, 0, 75)
      this.camera.lookAt(new THREE.Vector3(0, 0, 0))
      const controls = new OrbitControls(this.camera, this.renderer.domElement)
      // how far you can orbit vertically, upper and lower limits:
      controls.minPolarAngle = 0
      controls.maxPolarAngle = Math.PI
      // how far you can dolly in and out (PerspectiveCamera only):
      controls.minDistance = 0
      controls.maxDistance = Infinity
      controls.enableZoom = true
      controls.zoomSpeed = 1.0
      controls.enablePan = true
      controls.enableDamping = true 
      controls.dampingFactor = 0.25
      controls.addEventListener("change", this.render)
   }

   render () {
      this.renderer.render(this.scene, this.camera)
   }

   setPane (canvas: HTMLCanvasElement): void {
      this.scene.add(this.to3DTextureMap(canvas))
   }

   to3DTextureMap (canvas: HTMLCanvasElement): THREE.Object3D {
      const texture = new THREE.Texture(canvas)
      texture.needsUpdate = true
      const material = new THREE.MeshBasicMaterial({ map: texture }),
            geometry = new THREE.BoxGeometry(200, 200, 200)
      return new THREE.Mesh(geometry, material)
   }
}
