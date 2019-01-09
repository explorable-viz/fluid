// import { __nonNull } from "../src/util/Core"
// import { Profile, TestFile, τ, loadExample, runTest } from "../test/Helpers"

// const file: TestFile = loadExample("factorial")
// runTest(__nonNull(file.text), Profile.Match, τ.var_(null))

import * as THREE from "three"

const scene = new THREE.Scene()
const geometry = new THREE.BoxGeometry(1, 1, 1)
const material = new THREE.MeshBasicMaterial({ color: 0x00ff00 })
const cube = new THREE.Mesh( geometry, material)
scene.add(cube)

const camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000)
camera.position.z = 5

const renderer = new THREE.WebGLRenderer()
renderer.setSize(window.innerWidth, window.innerHeight)
document.body.appendChild(renderer.domElement)

const h = document.createElement("h1")
h.appendChild(document.createTextNode("Hello World"))  
document.body.appendChild(h)
