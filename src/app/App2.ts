import { __nonNull } from "../util/Core"
import { GraphicsPane3D2 } from "./GraphicsPane3D2"

const canvas: HTMLCanvasElement = document.createElement("canvas"),
      ctx = __nonNull(canvas.getContext('2d')),
      graphicsPane3D = new GraphicsPane3D2(window.innerWidth, window.innerHeight / 2)

function renderCanvas() {
   ctx.font = '20pt Arial'
   ctx.fillStyle = 'red'
   ctx.fillRect(0, 0, canvas.width, canvas.height)
   ctx.fillStyle = 'white'
   ctx.fillRect(10, 10, canvas.width - 20, canvas.height - 20)
   ctx.fillStyle = 'black'
   ctx.textAlign = "center"
   ctx.textBaseline = "middle"
   ctx.fillText(new Date().getTime().toString(), canvas.width / 2, canvas.height / 2)
}

function init() {
   document.body.appendChild(graphicsPane3D.renderer.domElement)
   graphicsPane3D.setCanvas(canvas)
   canvas.width = canvas.height = 256
}

function render() {
   renderCanvas()
   graphicsPane3D.texture.needsUpdate = true
   graphicsPane3D.mesh.rotation.y += 1
   graphicsPane3D.renderer.render(graphicsPane3D.scene, graphicsPane3D.camera)
}

init()
render()
