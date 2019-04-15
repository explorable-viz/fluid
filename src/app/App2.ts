import { __nonNull } from "../util/Core"
import { GraphicsPane3D2 } from "./GraphicsPane3D2"

class App2 {
   canvas: HTMLCanvasElement
   ctx: CanvasRenderingContext2D
   graphicsPane3D: GraphicsPane3D2
   
   constructor () {
      this.canvas = document.createElement("canvas")
      this.ctx = __nonNull(this.canvas.getContext('2d')),
      this.graphicsPane3D = new GraphicsPane3D2(window.innerWidth, window.innerHeight / 2)
   }

   init() {
      document.body.appendChild(this.graphicsPane3D.renderer.domElement)
      this.graphicsPane3D.setCanvas(this.canvas)
      this.canvas.width = this.canvas.height = 256
      this.render()
   }
   
   render() {
      this.renderCanvas()
      this.graphicsPane3D.texture.needsUpdate = true
      this.graphicsPane3D.mesh.rotation.y += 1
      this.graphicsPane3D.renderer.render(this.graphicsPane3D.scene, this.graphicsPane3D.camera)
   }

   renderCanvas() {
      this.ctx.font = '20pt Arial'
      this.ctx.fillStyle = 'red'
      this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)
      this.ctx.fillStyle = 'white'
      this.ctx.fillRect(10, 10, this.canvas.width - 20, this.canvas.height - 20)
      this.ctx.fillStyle = 'black'
      this.ctx.textAlign = "center"
      this.ctx.textBaseline = "middle"
      this.ctx.fillText(new Date().getTime().toString(), this.canvas.width / 2, this.canvas.height / 2)
   }
}

new App2().init()
