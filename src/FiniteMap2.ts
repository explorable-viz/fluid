import { Ord } from "./util/Ord"
import { Pair, Tree } from "./BaseTypes2"

export type FiniteMap<K extends Ord<K>, V> = Tree<Pair<K, V>>
