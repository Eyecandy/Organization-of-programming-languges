object GraphBFS extends  App{

  def bfs[V](nbrs: V => Set[V], src: V) = {
    def expand(frontier: Set[V], parent: Map[V, V]): (Set[V], Map[V, V]) = {
      val newFrontier: Set[V] = frontier.flatMap(v => nbrs(v)).filter(v => !parent.contains(v))
      val newParent: Map[V,V] =
        frontier.flatMap(v => nbrs(v).
            filter(u => !parent.contains(u)).
            flatMap(u => Map(u -> v) )).toMap ++ parent
      (newFrontier, newParent)
    }

    def iterate(frontier: Set[V], parent: Map[V, V], distance: Map[V, Int], d: Int): (Map[V,V], Map[V, Int]) =
      if (frontier.isEmpty)
        (parent, distance)
      else {
        val (frontier_, parent_) = expand(frontier, parent)
        val distance_ : Map[V,Int] = frontier_.flatMap(x => Map(x -> d)).toMap ++ distance
        iterate(frontier_, parent_, distance_, d + 1)
      }
    iterate(Set(src), Map(src -> src), Map(), 1)
  }
}
