type t = { now : Datetime.t }

let make ~now = { now }
let now ctx = ctx.now
