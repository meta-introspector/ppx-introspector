type __ = Obj.t

type ('t, 'p) total2 = { pr1 : 't; pr2 : 'p }
type ('x, 'y) dirprod = ('x, 'y) total2
type coq_UU = __
type precgraph =
  (coq_UU, (coq_UU, (__ -> __, __ -> __) dirprod) total2) total2
