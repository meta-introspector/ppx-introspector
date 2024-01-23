(*
First we take Univalent Mathematics 
@Misc{UniMath,
    author = {Voevodsky, Vladimir and Ahrens, Benedikt and Grayson, Daniel and others},
    title = {UniMath --- a computer-checked library of univalent mathematics},
    url = {https://github.com/UniMath/UniMath},
    howpublished = {available at \url{http://unimath.org}},
    doi          = {10.5281/zenodo.8427604},
    url          = {https://doi.org/10.5281/zenodo.8427604}
 }
Unimath [Coq](https://coq.inria.fr/) library aims to formalize a substantial body of mathematics using the
[univalent point of view](https://en.wikipedia.org/wiki/Univalent_foundations).

`paths_refl` is an inductive function that takes an element `a` from the type `A` and returns a value of type `UU`, which stands for "universe of universes." This function is responsible for returning a reference loop, which can be used to create an identity path.

First we extract unimath into ocaml using metacoq,
   then we extract the core types into ocaml.
   here are the foundations:
*)    
type __ = Obj.t
type hProptoType = __
type 'x isofhlevel = __
type 'x isaprop = 'x isofhlevel
type ('t, 'p) total2 = { pr1: 't; pr2 : 'p}
type 'a paths = | Coq_paths_refl (* terminal in proof system reflexive
                                    the proof system occupies this spot with a reflexive proof.
                                    witness to the truth.
                                    a path to truth. basically an identity in cat thoery, pointing at itself and saying I am true because I witnessed my truth.
                                 *)
(*"An isaset function takes in two elements, x and y, and returns a path connecting them."*)
type 'x isaset = 'x -> 'x -> 'x paths isaprop
    
type ('x, 'y) hfiber = ('x, 'y paths) total2 (*x leads to y reflexive*)


type ('x, 'y) isofhlevelf = 'y -> ('x, 'y) hfiber isofhlevel
type ('x, 'y) isincl = ('x, 'y) isofhlevelf
type coq_UU = __
type hProp = (coq_UU, __ isaprop) total2    
type coq_HLevel = (coq_UU, __ isofhlevel) total2
type pr1hSet = __
type node = __
type arc = __
type ('x, 'y) issurjective = 'y -> hProptoType
type 't iscontr = ('t, 't -> 't paths) total2
type ('x, 'y) isweq = 'y -> ('x, 'y) hfiber iscontr
type ('x, 'y) weq = ('x -> 'y, ('x, 'y) isweq) total2
type ('a, 'b) coq_PathPair = ('a paths, 'b paths) total2
type 'x istrans = 'x -> 'x -> 'x -> hProptoType -> hProptoType -> hProptoType
type 'x isrefl = 'x -> hProptoType
type 'x issymm = 'x -> 'x -> hProptoType -> hProptoType
type ('x, 'y) dirprod = ('x, 'y) total2
type 'x ispreorder = ('x istrans, 'x isrefl) dirprod
type 'x iseqrel = ('x ispreorder, 'x issymm) dirprod
type 'x hrel = 'x -> 'x -> hProp
type 'x eqrel = ('x hrel, 'x iseqrel) total2
type ('x, 'y) logeq = ('x -> 'y, 'y -> 'x) dirprod
type 'x hsubtype = 'x -> hProp
type 'x iseqclass =
  (hProptoType, ('x -> 'x -> hProptoType -> hProptoType -> hProptoType, 'x ->
  'x -> hProptoType -> hProptoType -> hProptoType) dirprod) dirprod
type 'x setquot = ('x hsubtype, 'x iseqclass) total2
type nat =
| O
| S of nat
type ('a, 'b) coprod =
| Coq_ii1 of 'a
| Coq_ii2 of 'b
type empty = |
type 'x neg = 'x -> empty

(*
    Now we want to condense these instructions
for processing the rest of the data.
we want to lift the ast types into unimath and construct types.
    so we can take the node type and create subtype for all the type of ast nodes.
     new types are born out of older types creating a left right relationship.
*)
