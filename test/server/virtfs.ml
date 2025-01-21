type 'a elt =
  { name : string
  ; mtime : int
  ; content : 'a
  }

type item =
  | File of string elt
  | Directory of t elt

and t = item list
