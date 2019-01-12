import * as THREE from "three"
import { __nonNull } from "../src/util/Core"
import { Profile, TestFile, τ, initialise, loadTestFile, runTest } from "../test/Helpers"

initialise()
const file: TestFile = loadTestFile("example", "factorial")
runTest(__nonNull(file.text), Profile.Match, τ.var_(null))

var scene = new THREE.Scene()
var camera = new THREE.PerspectiveCamera( 75, 1, 0.1, 1000 )

var renderer = new THREE.WebGLRenderer()
renderer.setSize( 600, 600 )
document.body.appendChild( renderer.domElement )

var geometry = new THREE.BoxGeometry( 1, 1, 1 )
var material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } )
var cube = new THREE.Mesh( geometry, material )
scene.add( cube )

camera.position.z = 5

renderer.render( scene, camera )
