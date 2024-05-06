open Gamelle

type particle = { pos : Point.t; speed : Vec.t; mass : float }
type settings = { n_bodies : int; g : float; friction : float; trace : bool }

type state = {
  is_in_menu : bool;
  settings : settings;
  particle_history : particle list list;
}

let button_box = Box.v (Vec.v 10. 10.) (Size.v 50. 50.)
let box = Box.v Vec.zero (Size.v 1000. 1000.)
let speed_range = Box.v (Point.v (-200.) (-200.)) (Size.v 400. 400.)
let collision_damping = 0.99
let circle_of_particle p = Circle.v p.pos (p.mass /. 500.)

let _solar_system =
  [
    { pos = Vec.v 0. 0.; speed = Vec.v 0. 0.; mass = 500000. };
    { pos = Vec.v 200. 0.; speed = Vec.v 0. 600.; mass = 5000. };
    { pos = Vec.v 3000. 0.; speed = Vec.v 0. 375.; mass = 10000. };
    { pos = Vec.v 3100. 0.; speed = Vec.v 0. 625.; mass = 200. };
    { pos = Vec.v 5000. 0.; speed = Vec.v 0. 300.; mass = 50000. };
    { pos = Vec.v 5250. 0.; speed = Vec.v 0. 750.; mass = 2500. };
    { pos = Vec.v 5500. 0.; speed = Vec.v 0. 650.; mass = 5000. };
    (* { *)
    (*   pos = Vec.v 5550. 0.; *)
    (*   speed = Vec.v 0. 950.; *)
    (*   mass = 1000.; *)
    (* }; *)
  ]

let init_particles { n_bodies; g = _; trace = _; friction = _ } =
  List.init n_bodies (fun _i ->
      {
        pos = Box.random_mem box;
        speed = Box.random_mem speed_range;
        mass = 10000.;
      })

let init_settings = { n_bodies = 3; g = 1000.; friction = 0.9; trace = false }

let init () =
  {
    is_in_menu = false;
    settings = init_settings;
    particle_history = [ init_particles init_settings ];
  }

let gravity_force ~g p1 p2 =
  let d = Vec.(norm (p2.pos - p1.pos)) in
  if d = 0. then Vec.zero
  else
    let d =
      Float.max
        (Circle.radius (circle_of_particle p1)
        +. Circle.radius (circle_of_particle p2))
        d
    in
    let i = g *. p1.mass *. p2.mass /. (d *. d) in
    Vec.(i * unit (p2.pos - p1.pos))

let gravity_forces ~g p particles =
  List.fold_left Vec.( + ) Vec.zero (List.map (gravity_force ~g p) particles)

let collision ~friction p1 p2 =
  let c1 = circle_of_particle p1 and c2 = circle_of_particle p2 in
  if (not (Circle.intersects c1 c2)) || p1 = p2 then None
  else
    let { pos = x1; speed = v1; mass = m1 } = p1
    and { pos = x2; speed = v2; mass = m2 } = p2 in
    let open Vec in
    let speed =
      friction
      * (v1
        - 2. *. m2 /. (m1 +. m2)
          *. (dot (v1 - v2) (x1 - x2) /. norm2 (x1 - x2))
          * (x1 - x2))
    in
    (* TODO fix collapsed particles flying away when switching window *)
    let pos =
      Vec.(((Circle.radius c1 +. Circle.radius c2) * unit (x1 - x2)) + x2)
    in
    Some { pos; speed; mass = m1 }

let collisions ~friction p particles =
  match List.find_map (collision ~friction p) particles with
  | Some p -> p
  | None -> p

let update_particle ~settings particles p =
  let { pos; speed; mass } =
    collisions ~friction:settings.friction p particles
  in
  let speed =
    Vec.(speed + (dt () * (gravity_forces ~g:settings.g p particles / mass)))
  in
  let new_pos = Vec.(pos + (dt () * speed)) in
  { pos = new_pos; speed; mass }

let update_particles ~settings particles =
  List.map (update_particle ~settings particles) particles

let rec bound_list i li =
  if i = 0 then []
  else match li with [] -> [] | elt :: li -> elt :: bound_list (i - 1) li

let update_history ~settings state =
  bound_list (60 * 20) @@ (update_particles ~settings (List.hd state) :: state)

let render_particle ~io ~color p = Circle.fill ~io ~color (circle_of_particle p)

let centre_of_mass particles =
  let total_mass = List.fold_left (fun acc p -> acc +. p.mass) 0. particles in
  Vec.(
    List.fold_left ( + ) zero
      (List.map (fun p -> Vec.(p.mass * p.pos)) particles)
    / total_mass)

let get_drawing_box particles =
  let padding = 50. in
  let mid = centre_of_mass particles in
  let dx, dy =
    List.fold_left
      (fun (dx, dy) p ->
        let dp = Vec.(mid - p.pos) in
        ( Float.max dx (Float.abs (Vec.x dp)),
          Float.max dy (Float.abs (Vec.y dp)) ))
      (500., 500.) particles
  in
  let biggest_radius =
    List.fold_left
      (fun candidate p ->
        let radius = Circle.radius (circle_of_particle p) in
        Float.max radius candidate)
      0. particles
  in

  let padding = biggest_radius +. padding in
  let dx = Float.max (dx +. padding) 100.
  and dy = Float.max (dy +. padding) 100. in
  Box.v_mid mid (Size.v (dx *. 2.) (dy *. 2.))

let render_particles ~io i particles =
  let io = View.z_indexed (-i) io in
  let color =
    let open Color in
    let alpha = 1. /. (float_of_int i +. 1.) in
    let color = blend (with_a white alpha) red in
    Color.(with_a color alpha)
  in
  (* Box.draw ~io ~color (get_drawing_box particles); *)
  (* Circle.fill ~io ~color:Color.red (Circle.v Vec.zero 1.); *)
  List.iter (render_particle ~io ~color) particles

let render ~io state =
  (let io =
     io
     |> View.drawing_box ~scale:true ~set_window_size:false
          (get_drawing_box (List.hd state.particle_history))
   in
   List.iteri (render_particles ~io)
     (if state.settings.trace then state.particle_history
      else [ List.hd state.particle_history ]));
  Box.fill ~io ~color:Color.red button_box;
  Box.draw ~io ~color:Color.white button_box

let menu ~io =
  let (reset, close, settings), _box =
    Ui.(
      window ~io (Vec.v 10. 10.) (fun ui ->
          label [%ui] ~style:Style.(horizontal Center) "Settings";
          let trace = checkbox [%ui] "Trace" in
          let n_bodies = int_slider [%ui] ~init:3 ~min:1 ~max:7 in
          label [%ui] (Printf.sprintf "Bodies : %i" n_bodies);
          let g = slider [%ui] ~init:1000. ~min:750. ~max:1500. in
          label [%ui] (Printf.sprintf "G : %f" g);
          let friction =
            slider [%ui] ~init:init_settings.friction ~min:0. ~max:1.
          in
          label [%ui] (Printf.sprintf "Friction : %f" friction);
          let reset, close =
            horizontal [%ui] (fun () ->
                (button [%ui] "Reset", button [%ui] "Close"))
          in
          (reset, close, { n_bodies; g; friction; trace })))
  in
  (reset, close, settings)

let update ~io state =
  if state.is_in_menu then
    let reset, close, settings = menu ~io in
    let state = { state with settings } in
    let state =
      if reset then
        { state with particle_history = [ init_particles settings ] }
      else state
    in
    let is_in_menu = if reset || close then false else true in
    { state with is_in_menu }
  else if
    Event.is_down ~io `click_left && Box.mem (Event.mouse_pos ~io) button_box
  then { state with is_in_menu = true }
  else
    {
      state with
      particle_history =
        update_history ~settings:state.settings state.particle_history;
    }

let () =
  let () = Random.self_init () in
  let state = init () in
  Gamelle.run (state, state) @@ fun ~io (start, state) ->
  (* Window.set_size ~io (Size.v 1000. 1000.); *)
  show_cursor ~io true;
  if Event.is_pressed ~io `escape then raise Exit;
  (* restart *)
  let state = if Event.is_down ~io (`input_char "r") then start else state in
  (* reload *)
  let start, state =
    if Event.is_down ~io (`input_char "R") then
      let state = init () in
      (state, state)
    else (start, state)
  in
  render ~io state;
  let state = update ~io state in
  (start, state)
