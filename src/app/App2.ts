import * as THREE from "three"
import { __nonNull } from "../util/Core"
import { GraphicsPane3D2 } from "./GraphicsPane3D2"

var width = window.innerWidth, height = window.innerHeight / 2;
var size = 256;
var canvas: HTMLCanvasElement = document.createElement("canvas"),
    ctx = __nonNull(canvas.getContext('2d')),
    graphicsPane3D = new GraphicsPane3D2
function renderCanvas() {
   ctx.font = '20pt Arial';
   ctx.fillStyle = 'red';
   ctx.fillRect(0, 0, canvas.width, canvas.height);
   ctx.fillStyle = 'white';
   ctx.fillRect(10, 10, canvas.width - 20, canvas.height - 20);
   ctx.fillStyle = 'black';
   ctx.textAlign = "center";
   ctx.textBaseline = "middle";
   ctx.fillText(new Date().getTime().toString(), canvas.width / 2, canvas.height / 2);
}
function init() {
   graphicsPane3D.renderer = new THREE.WebGLRenderer();
   graphicsPane3D.renderer.setSize(width, height);
   document.body.appendChild(graphicsPane3D.renderer.domElement);

   graphicsPane3D.scene = new THREE.Scene();

   graphicsPane3D.camera = new THREE.PerspectiveCamera(70, width / height, 1, 1000);
   graphicsPane3D.camera.position.z = 500;
   graphicsPane3D.scene.add(graphicsPane3D.camera);
   graphicsPane3D.texture = new THREE.Texture(canvas);
   var material = new THREE.MeshBasicMaterial({ map: graphicsPane3D.texture });
   graphicsPane3D.geometry = new THREE.BoxGeometry( 200, 200, 200 );
   graphicsPane3D.mesh = new THREE.Mesh( graphicsPane3D.geometry, material );
   graphicsPane3D.scene.add( graphicsPane3D.mesh );
   canvas.width = canvas.height = size;
}

function render() {
   renderCanvas();
   graphicsPane3D.texture.needsUpdate = true;
   graphicsPane3D.mesh.rotation.y += 1;
   graphicsPane3D.renderer.render(graphicsPane3D.scene, graphicsPane3D.camera);
}

init();
render();
