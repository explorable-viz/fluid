import * as THREE from "three"
import { absurd, assert } from "../util/Core"
import { Cons, List } from "../BaseTypes"
import { Graphic, GraphicsElement, LinearTransform, PathStroke, Point, RectFill, Scale, Transform, Translate, Transpose } from "../Graphics"

export function to3DTextureMap (canvas: HTMLCanvasElement): THREE.Object3D {
   const texture = new THREE.Texture(canvas),
   material = new THREE.MeshBasicMaterial({ map: texture }),
   geometry = new THREE.BoxGeometry(200, 200, 200)
   return new THREE.Mesh(geometry, material)
}

type TransformFun = (p: [number, number]) => [number, number]

export class GraphicsRenderer {
   transforms: TransformFun[] // stack of successive compositions of linear transformations
   ctx: CanvasRenderingContext2D

   constructor (ctx: CanvasRenderingContext2D) {
      this.ctx = ctx
      this.transforms = [([x, y]) => [x * 5, 800 -(y * 5)]] // TODO: fix
   }

   get transform (): TransformFun {
      assert(this.transforms.length > 0)
      return this.transforms[this.transforms.length - 1]
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
         this.pathStroke(g.points)
      } else
      if (g instanceof RectFill) {
         this.rectFill(g.points)
      } else
      if (g instanceof Transform) {
         // TODO: factor out common handling.
         const t: LinearTransform = g.t
         if (t instanceof Scale) {
            const transform: TransformFun = this.transform
            this.transforms.push(([x, y]): [number, number] => {
               return transform([x * t.x.n, y * t.y.n])
            })
            this.renderElement(g.g)
            this.transforms.pop()
         } else
         if (t instanceof Translate) {
            const transform: TransformFun = this.transform
            this.transforms.push(([x, y]): [number, number] => {
               return transform([x + t.x.n, y + t.y.n])
            })
            this.renderElement(g.g)
            this.transforms.pop()
         } else
         if (t instanceof Transpose) {
            const transform: TransformFun = this.transform
            this.transforms.push(([x, y]): [number, number] => {
               return transform([y, x])
            })
            this.renderElement(g.g)
            this.transforms.pop()
         } else {
            return absurd()
         }
      }
      else {
         return absurd()
      }
   }

   path2D (points: List<Point>): Path2D {
      const region: Path2D = new Path2D
      if (Cons.is(points)) {
         const [x, y]: [number, number] = this.transform([points.head.x.n, points.head.y.n])
         region.moveTo(x, y)
         points = points.tail
         for (; Cons.is(points); points = points.tail) {
            const [x, y]: [number, number] = this.transform([points.head.x.n, points.head.y.n])
            region.lineTo(x, y)
         }
      } else {
         return absurd()
      }
      return region
   }

   pathStroke (points: List<Point>): void {
      const region: Path2D = this.path2D(points)
      this.ctx.strokeStyle = "black"
      this.ctx.stroke(region)
      this.pointHighlights(points)
   }

   pointHighlights (points: List<Point>): void {
      for (; Cons.is(points); points = points.tail) {
         const point: Point = points.head,
               [x, y]: [number, number] = this.transform([point.x.n, point.y.n])
         if (!point.x.α || !point.y.α) {
            this.circle(x, y, 3)
         }
      }
   }

   circle (x: number, y: number, radius: number): void {
      this.ctx.beginPath()
      this.ctx.arc(x, y, radius, 0, 2 * Math.PI)
      this.ctx.strokeStyle = "#0000ff"
      this.ctx.stroke()
   }

   rectFill (rect_path: List<Point>): void {
      const region: Path2D = this.path2D(rect_path)
      this.ctx.fillStyle = "#F6831E"
      this.ctx.fill(region)
   }
}
