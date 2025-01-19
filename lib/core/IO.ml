type 'a t = unit -> 'a

let return x () = x
let bind x f = f (x ())
let map f x = bind x (fun m -> return @@ f m)
let apply ft xt = map (ft ()) xt
let zip x y = apply (map (fun a b -> a, b) x) y

module Syntax = struct
  let ( let+ ) x f = map f x
  let ( and+ ) = zip
  let ( let* ) = bind
end

include Syntax

let perform raw_effect () = Effect.perform raw_effect
let run handler input = handler (input ())
let force x = run (fun x -> x) x
let compose handler1 handler2 p () = handler1 (run handler2 p)
