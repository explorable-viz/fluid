import * as THREE from "three"
import { __nonNull } from "../src/util/Core"
import { Profile, TestFile, τ, initialise, loadTestFile, runTest } from "../test/Helpers"

initialise()
const file: TestFile = loadTestFile("example", "factorial")
runTest(__nonNull(file.text), Profile.Match, τ.var_(null))

var scene = new THREE.Scene()
var camera = new THREE.PerspectiveCamera( 45, 1, 1, 500 )
camera.position.set( 0, 0, 100 );
camera.lookAt( new THREE.Vector3(0, 0, 0) );

var renderer = new THREE.WebGLRenderer()
renderer.setSize( 600, 600 )
document.body.appendChild( renderer.domElement )

var geometry = new THREE.Geometry()
geometry.vertices.push(new THREE.Vector3( -10, 0, 0) )
geometry.vertices.push(new THREE.Vector3( 0, 10, 0) )
geometry.vertices.push(new THREE.Vector3( 10, 0, 0) )
var material = new THREE.LineBasicMaterial( { color: 0x0000ff } )
var line = new THREE.Line( geometry, material )
scene.add(line)

renderer.render( scene, camera )
