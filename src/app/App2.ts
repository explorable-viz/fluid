import * as THREE from "three"
import { __nonNull } from "../util/Core"

var width = window.innerWidth, height = window.innerHeight / 2;
var size = 256;
var canvas: HTMLCanvasElement = document.createElement("canvas"),
    ctx = __nonNull(canvas.getContext('2d'));
var camera: THREE.Camera, scene: THREE.Scene, renderer: THREE.WebGLRenderer, geometry, texture: THREE.Texture, mesh: THREE.Mesh;
function changeCanvas() {
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
    renderer = new THREE.WebGLRenderer();
    renderer.setSize(width, height);
    document.body.appendChild(renderer.domElement);
    
    scene = new THREE.Scene();
  
    camera = new THREE.PerspectiveCamera(70, width / height, 1, 1000);
    camera.position.z = 500;
    scene.add(camera);
    texture = new THREE.Texture(canvas);
    var material = new THREE.MeshBasicMaterial({ map: texture });
    geometry = new THREE.BoxGeometry( 200, 200, 200 );
    mesh = new THREE.Mesh( geometry, material );
    scene.add( mesh );
    canvas.width = canvas.height = size;
}
function animate() {
    requestAnimationFrame(animate);
  
    changeCanvas();
    texture.needsUpdate = true;
    mesh.rotation.y += 0.01;
    renderer.render(scene, camera);
}
init();
animate();
