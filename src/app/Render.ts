import * as THREE from "three"
import { absurd, as, assert } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { Graphic, GraphicsElement, PathStroke, Point, RectFill, Scale, Translate, Transpose } from "../Graphics"

type Transform = (p: THREE.Vector2) => THREE.Vector2

// TODO: rename to avoid conceptual clash with WebGL canvas.
export class Canvas3D {
   transforms: Transform[] // stack of successive compositions of linear transformations

   constructor () {
      this.transforms = [x => x]
   }

   get transform (): Transform {
      assert(this.transforms.length > 0)
      return this.transforms[this.transforms.length - 1]
   }

   objects3D (elem: GraphicsElement): THREE.Object3D[] {
      if (elem instanceof Graphic) {   
         const objects: THREE.Object3D[] = []
         for (let elems: List<GraphicsElement> = elem.elems; Cons.is(elems); elems = elems.tail) {
            objects.push(...this.objects3D(elems.head))
         }
         return objects
      }
      if (elem instanceof PathStroke) {
         return this.pathStroke(elem.points)
      } else
      if (elem instanceof RectFill) {
         return this.rectFill(elem.points)
      } else
      // TODO: factor out common handling.
      if (elem instanceof Scale) {
         const transform: Transform = this.transform
         this.transforms.push(({x, y}): THREE.Vector2 => {
            return transform(new THREE.Vector2(x * elem.x.n, y * elem.y.n))
         })
         const objects: THREE.Object3D[] = this.objects3D(elem.elem)
         this.transforms.pop()
         return objects
      } else
      if (elem instanceof Translate) {
         const transform: Transform = this.transform
         this.transforms.push(({x, y}): THREE.Vector2 => {
            return transform(new THREE.Vector2(x + elem.x.n, y + elem.y.n))
         })
         const objects: THREE.Object3D[] = this.objects3D(elem.elem)
         this.transforms.pop()
         return objects
      } else
      if (elem instanceof Transpose) {
         const transform: Transform = this.transform
         this.transforms.push(({x, y}): THREE.Vector2 => {
            return transform(new THREE.Vector2(y, x))
         })
         const objects: THREE.Object3D[] = this.objects3D(elem.elem)
         this.transforms.pop()
         return objects
      } else {
         return absurd()
      }
   }

   pathStroke (points: List<Point>): THREE.Object3D[] {
      const stroke: THREE.Line = new THREE.Line(
         this.newPathGeometry(points),
         new THREE.LineBasicMaterial({ 
            color: 0x000000 
         })
      )
      return [stroke, ...this.pointHighlights(points)]
   }

   rectFill (rect_path: List<Point>): THREE.Object3D[] {
      const geometry: THREE.Geometry = this.newPathGeometry(rect_path)
      geometry.faces.push(new THREE.Face3(0, 1, 2))
      geometry.faces.push(new THREE.Face3(2, 3, 0))
      return [new THREE.Mesh(
         geometry, 
         new THREE.MeshBasicMaterial({ color: 0xF6831E, side: THREE.DoubleSide })
      )]
   }

   newPathGeometry (points: List<Point>): THREE.Geometry {
      const geometry: THREE.Geometry = new THREE.Geometry,
            transform: Transform = this.transform
      while (Cons.is(points)) {
         const point: Point = as(points.head, Point),
               {x, y}: THREE.Vector2 = transform(new THREE.Vector2(point.x.n, point.y.n))
         geometry.vertices.push(new THREE.Vector3(x, y, 0))
         points = points.tail
      }
      return geometry
   }   

   pointHighlights (points: List<Point>): THREE.Object3D[] {
      const highlights: THREE.Object3D[] = [],
            transform: Transform = this.transform
      for (; Cons.is(points); points = points.tail) {
         const point: Point = points.head,
               p: THREE.Vector2 = transform(new THREE.Vector2(point.x.n, point.y.n))
         if (!point.x.α || !point.y.α) {
            highlights.push(circle(p, 0.5))
         }
      }
      return highlights
   }
}

function circle (pos: THREE.Vector2, radius: number): THREE.Object3D {
   const material = new THREE.LineBasicMaterial({ color: 0x0000ff }),
         geometry = new THREE.CircleGeometry(radius, 64)
   geometry.vertices.shift() // remove center vertex
   const circle: THREE.LineLoop = new THREE.LineLoop(geometry, material)
   circle.position.x = pos.x
   circle.position.y = pos.y
   return circle
}
