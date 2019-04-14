import * as THREE from "three"
import { absurd, as, assert } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { Graphic, GraphicsElement, LinearTransform, PathStroke, Point, RectFill, Scale, Transform, Translate, Transpose } from "../Graphics"

export function to3DTextureMap (canvas: HTMLCanvasElement): THREE.Object3D {
   const texture = new THREE.Texture(canvas),
   material = new THREE.MeshBasicMaterial({ map: texture }),
   geometry = new THREE.BoxGeometry(200, 200, 200)
   return new THREE.Mesh(geometry, material)
}

type TransformFun = (p: THREE.Vector2) => THREE.Vector2
type TransformFun2 = (p: [number, number]) => [number, number]

export class Renderer {
   transforms: TransformFun[] // stack of successive compositions of linear transformations
   transforms2: TransformFun2[] // stack of successive compositions of linear transformations
   ctx: CanvasRenderingContext2D

   constructor (ctx: CanvasRenderingContext2D) {
      this.ctx = ctx
      this.transforms = [x => x]
      this.transforms2 = [([x, y]) => [x, y]]
   }

   get transform (): TransformFun {
      assert(this.transforms.length > 0)
      return this.transforms[this.transforms.length - 1]
   }

   get transform2 (): TransformFun2 {
      assert(this.transforms2.length > 0)
      return this.transforms2[this.transforms2.length - 1]
   }

   render (g: GraphicsElement): THREE.Object3D {
      this.renderElement(g)
      return to3DTextureMap(this.ctx.canvas)
   }

   renderElement (g: GraphicsElement): void {
      if (g instanceof Graphic) {   
         for (let gs: List<GraphicsElement> = g.gs; Cons.is(gs); gs = gs.tail) {
            this.renderElement(gs.head)
         }
      } else 
      if (g instanceof PathStroke) {
         this.pathStroke2(g.points)
      } else
      if (g instanceof RectFill) {
         this.rectFill2(g.points)
      } else
      if (g instanceof Transform) {
         // TODO: factor out common handling.
         const t: LinearTransform = g.t
         if (t instanceof Scale) {
            const transform: TransformFun2 = this.transform2
            this.transforms2.push(([x, y]): [number, number] => {
               return transform([x * t.x.n, y * t.y.n])
            })
            this.renderElement(g.g)
            this.transforms2.pop()
         } else
         if (t instanceof Translate) {
            const transform: TransformFun2 = this.transform2
            this.transforms2.push(([x, y]): [number, number] => {
               return transform([x + t.x.n, y + t.y.n])
            })
            this.renderElement(g.g)
            this.transforms2.pop()
         } else
         if (t instanceof Transpose) {
            const transform: TransformFun2 = this.transform2
            this.transforms2.push(([x, y]): [number, number] => {
               return transform([y, x])
            })
            this.renderElement(g.g)
            this.transforms2.pop()
         } else {
            return absurd()
         }
      }
      else {
         return absurd()
      }
   }

   objects3D (g: GraphicsElement): THREE.Object3D[] {
      if (g instanceof Graphic) {   
         const objects: THREE.Object3D[] = []
         for (let gs: List<GraphicsElement> = g.gs; Cons.is(gs); gs = gs.tail) {
            objects.push(...this.objects3D(gs.head))
         }
         return objects
      } else
      if (g instanceof PathStroke) {
         return this.pathStroke(g.points)
      } else
      if (g instanceof RectFill) {
         return this.rectFill(g.points)
      } else
      if (g instanceof Transform) {
         // TODO: factor out common handling.
         const t: LinearTransform = g.t
         if (t instanceof Scale) {
            const transform: TransformFun = this.transform
            this.transforms.push(({x, y}): THREE.Vector2 => {
               return transform(new THREE.Vector2(x * t.x.n, y * t.y.n))
            })
            const objects: THREE.Object3D[] = this.objects3D(g.g)
            this.transforms.pop()
            return objects
         } else
         if (t instanceof Translate) {
            const transform: TransformFun = this.transform
            this.transforms.push(({x, y}): THREE.Vector2 => {
               return transform(new THREE.Vector2(x + t.x.n, y + t.y.n))
            })
            const objects: THREE.Object3D[] = this.objects3D(g.g)
            this.transforms.pop()
            return objects
         } else
         if (t instanceof Transpose) {
            const transform: TransformFun = this.transform
            this.transforms.push(({x, y}): THREE.Vector2 => {
               return transform(new THREE.Vector2(y, x))
            })
            const objects: THREE.Object3D[] = this.objects3D(g.g)
            this.transforms.pop()
            return objects
         } else {
            return absurd()
         }
      } else {
         return absurd()
      }
   }

   path2D (points: List<Point>): Path2D {
      const region: Path2D = new Path2D,
            transform: TransformFun2 = this.transform2
      if (Cons.is(points)) {
         const [x, y]: [number, number] = transform([points.head.x.n, points.head.y.n])
         region.moveTo(x, y)
         points = points.tail
         for (; Cons.is(points); points = points.tail) {
            const [x, y]: [number, number] = transform([points.head.x.n, points.head.y.n])
            region.lineTo(x, y)
         }
      } else {
         return absurd()
      }
      return region
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

   pathStroke2 (points: List<Point>): void {
      const region: Path2D = this.path2D(points)
      this.ctx.strokeStyle = "black"
      this.ctx.stroke(region)
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

   rectFill2 (rect_path: List<Point>): void {
      const region: Path2D = this.path2D(rect_path)
      this.ctx.fillStyle = "green"
      this.ctx.fill(region)
   }

   newPathGeometry (points: List<Point>): THREE.Geometry {
      const geometry: THREE.Geometry = new THREE.Geometry,
            transform: TransformFun = this.transform
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
            transform: TransformFun = this.transform
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
