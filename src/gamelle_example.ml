open Gamelle

type particle = { pos : Point.t; speed : Vec.t; mass : float }
type state = particle list

let box = Box.v Vec.zero (Size.v 1000. 1000.)
let speed_range = Box.v (Point.v (-50.) (-50.)) (Size.v 100. 100.)
let num_part = 3
let () = Random.self_init ()
let g = 1000.
let collision_damping = 0.9
let circle_of_particle p = Circle.v p.pos (p.mass /. 100.)

let init =
  List.init num_part (fun _i ->
  {
    pos = Box.random_mem box;
    speed = Box.random_mem speed_range;
    mass = Random.float 5000. +. 2000.;
  })
  (* [ *)
  (*   { pos = Point.v 300. 600.; speed = Vec.v 100. 0.; mass = 2000. }; *)
  (*   { pos = Point.v 700. 500.; speed = Vec.v 0. 0.; mass = 10000. }; *)
  (* ] *)
(* @ [ { *)
(*     pos = Point.v 500. 500.; *)
(*     speed = Vec.v 0. 0.; *)
(*     mass = 1000. *)
(*   } ] *)

let gravity_force p1 p2 =
  let d = Vec.(norm @@ (p2.pos - p1.pos)) in
  if d = 0. then Vec.zero
  else
    let i = g *. p1.mass *. p2.mass /. (d *. d) in
    Vec.(i * unit (p2.pos - p1.pos))

let gravity_forces p particles =
  List.fold_left Vec.( + ) Vec.zero (List.map (gravity_force p) particles)

let collision p1 p2 =
  let c1 = circle_of_particle p1 and c2 = circle_of_particle p2 in
  if (not (Circle.intersects c1 c2)) || p1 = p2 then None
  else
    let { pos = x1; speed = v1; mass = m1 } = p1
    and { pos = x2; speed = v2; mass = m2 } = p2 in
    let open Vec in
    let speed = collision_damping * (v1
      - 2. *. m2 /. (m1 +. m2)
        *. (dot (v1 - v2) (x1 - x2) /. norm2 (x1 - x2))
        * (x1 - x2)) in
    (* TODO fix collapsed particles flying away when switching window *)
    let pos = Vec.((Circle.radius c1 +. Circle.radius c2) * (unit (x1 - x2)) + x2) in
    Some { pos; speed; mass = m1 }

let collisions p particles =
  match List.find_map (collision p) particles with
  | Some p -> p
  | None -> p

let update_particle particles p =
  let { pos; speed; mass } = collisions p particles in
  let speed = Vec.(speed + (dt () * (gravity_forces p particles / mass))) in
  let new_pos = Vec.(pos + (dt () * speed)) in
  { pos = new_pos; speed; mass }

let update state = List.map (update_particle state) state

let render_particle ~io p =
  Circle.fill ~io ~color:Color.white (circle_of_particle p)

let centre_of_mass particles =
  let total_mass = List.fold_left (fun acc p -> acc +. p.mass) 0. particles in
  Vec.(
    List.fold_left ( + ) zero
      (List.map (fun p -> Vec.(p.mass * p.pos)) particles)
    / total_mass)

let get_drawing_box particles =
  let mid = centre_of_mass particles in
  let dx, dy =
    List.fold_left
      (fun (dx, dy) p ->
        let dp = Vec.(mid - p.pos) in
        ( Float.max dx (Float.abs (Vec.x dp)),
          Float.max dy (Float.abs (Vec.y dp)) ))
      (0., 0.) particles
  in
  (* let dx = Float.max (dx +. 100.) 500. and dy = Float.max (dy +. 100.) 500. in *)
  Box.v_mid mid (Size.v (dx *. 2.) (dy *. 2.))

let render ~io state =
  let io = View.drawing_box (get_drawing_box state) io in
  Box.draw ~io ~color:Color.white (get_drawing_box state);
  Circle.fill ~io ~color:Color.red (Circle.v Vec.zero 1.);
  List.iter (render_particle ~io) state

let () =
  Gamelle.run init @@ fun ~io state ->
  if Event.is_pressed ~io `escape then raise Exit;
  let state = update state in
  render ~io state;
  state
