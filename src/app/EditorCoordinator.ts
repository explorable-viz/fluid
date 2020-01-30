import { Placement } from "tippy.js"
import { assert } from "../util/Core"
import { Direction, Slice, __slice } from "../Annotation"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { Editor } from "./Editor"
import { View } from "./View"

export class EditorCoordinator implements Editor.Listener {
   ρ_external: Env
   editors: Set<Editor.Editor> = new Set()

   constructor (ρ_external: Env) {
      this.ρ_external = ρ_external
   }

   addEditor (ρ: Env, e: Expr, tooltipPlacement: Placement = "top"): Editor.Editor {
      const editor: Editor.Editor = new Editor.Editor(this, View.defaultDims, tooltipPlacement, this.ρ_external, ρ, e)
      editor.initialise()
      this.editors.add(editor)
      return editor
   }

   removeEditor (editor: Editor.Editor): void {
      assert(this.editors.has(editor))
      this.editors.delete(editor)
   }

   onBwdSlice (editor: Editor.Editor, externDeps: Slice): void {
      // consider availability of ρ_external only; treat ρ and e as unlimited resources
      [...this.editors]
         .filter(editor_ => editor_ !== editor)
         .forEach((editor_: Editor.Editor): void => {
            editor_.fwdSlice(externDeps)
            editor_.render() // TODO: just redo selection rendering
         })
      __slice.reset(Direction.Fwd)
   }
}
