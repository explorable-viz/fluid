import * as THREE from "three"
import { OrbitControls } from "three-orbitcontrols-ts"

export class GraphicsPane3D {
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
      this.scene.background = new THREE.Color(0xffffff)
      this.camera = new THREE.PerspectiveCamera(
         /* field of view (degrees) */ 30,
         /* aspect ratio */            width / height, 
         /* near */                    1, 
         /* far */                     1000
      )
      this.camera.position.z = 1000
      this.scene.add(this.camera)

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
      controls.addEventListener("change", () => { this.render() })
   }

   setCanvas (canvas: HTMLCanvasElement): void {
      this.texture = new THREE.Texture(canvas)
      const material = new THREE.MeshBasicMaterial({ map: this.texture })
      this.geometry = new THREE.BoxGeometry(200, 200, 200)
      this.mesh = new THREE.Mesh(this.geometry, material)
      this.scene.add(this.mesh)
   }

   render (): void {
      this.texture.needsUpdate = true
      this.renderer.render(this.scene, this.camera)
   }
}
