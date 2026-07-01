open Graphics

module LabelMap = Map.Make(String)

(* ── Types ────EXTEND THIS TYPE WITH MORE OPERATIONS ─────────────────────────────── *)

type instr =
  | Square       of string * int * int * int * int         (* color x y w h *)
  | Line         of string * int * int * int * int * int   (* color lw x1 y1 x2 y2 *)
  | Image        of string * int * int * float * float     (* file x y scale rotation_deg *)
  | Text         of string * int * int * int * string      (* color x y size text *)
  | Call         of string                                  (*Instrucao a executar*)
  |Translate     of float * float                            (*dx e dy, novas origens*)
  |Rotate        of float                                     (*degrees*)
  |RepeatRotate  of string*int*float                          (*label, n ,deg*)
  |RepeatPlanar  of string *int*int*float*float*float 

type program = instr list LabelMap.t

(* ── Transform ────DO NOT CHANGE ─────────────────────────────────────────────────── *)

type referential = { ox: float; oy: float; angle: float }  (* angle in radians *)

let origin = { ox = 0.0; oy = 0.0; angle = 0.0 }

let _apply_translate t dx dy =
  { t with
    ox = t.ox +. cos t.angle *. dx -. sin t.angle *. dy;
    oy = t.oy +. sin t.angle *. dx +. cos t.angle *. dy }

let _apply_rotate t deg =
  { t with angle = t.angle +. deg *. Float.pi /. 180.0 }

(* Map a local integer point to screen pixels via the current transform. *)
let screen sv t x y =
  let fx = float_of_int x and fy = float_of_int y in
  let fsv = float_of_int sv in
  let cx = (t.ox +. cos t.angle *. fx -. sin t.angle *. fy) *. fsv in
  let cy = (t.oy +. sin t.angle *. fx +. cos t.angle *. fy) *. fsv in
  (int_of_float (Float.round cx), int_of_float (Float.round cy))

(* ── Parsing ─────EXTEND WITH MORE OPERATIONS ─────────────────────────────── *)

let color_of_string = function
  | "black"  -> Graphics.black
  | "white"  -> Graphics.white
  | "red"    -> Graphics.red
  | "blue"   -> Graphics.blue
  | "green"  -> Graphics.green
  | "yellow" -> 0xFFFF00
  | s        -> failwith ("unknown color: " ^ s)

let words s =
  String.split_on_char ' ' s |> List.filter ((<>) "")

(* 
  This function recognizes the instructions from a line of text 
  EXTEND WITH MORE CASES
*)
let parse_instr s =
  match words s with
  | ["square"; c; x; y; w; h]         ->
    Square (c, int_of_string x, int_of_string y,
    int_of_string w, int_of_string h)
  | ["line"; c; lw; x1; y1; x2; y2] ->
    Line (c, int_of_string lw,
    int_of_string x1, int_of_string y1,
    int_of_string x2, int_of_string y2)
  | ["image"; file; x; y; scale; rot] ->
    Image (file,
    int_of_string x, int_of_string y,
    float_of_string scale, float_of_string rot)
    | "text" :: c :: x :: y :: size :: rest when rest <> [] ->
      Text (c, int_of_string x, int_of_string y,
      int_of_string size, String.concat " " rest)
  
  |["call"; label] -> Call label

  |["translate"; dx;dy] ->
    Translate (float_of_string dx, float_of_string dy)

  |["rotate"; deg] ->
    Rotate (float_of_string deg)

  |["repeatrotate"; label ;n;deg] ->
    RepeatRotate (label, int_of_string n, float_of_string deg)

  |["repeatplanar";label;nx;ny;dx;dy;sy] ->
    RepeatPlanar (label, int_of_string nx, int_of_string ny, float_of_string dx, float_of_string dy, float_of_string sy)
  | _ -> failwith ("unknown instruction: " ^ s)

(* DO NOT CHANGE *)
let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line                  -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

(* DO NOT CHANGE *)
let add_to_map lbl acc map =
  match lbl with
  | None   -> map
  | Some l -> LabelMap.add l (List.rev acc) map

(* 
  This function iterates the list of lines from the input file and
  if detects a label stores it in a map that starts empty and accumulates 
  all the blocks. DO NOT CHANGE
*)
let parse_lines lines =
    lines 
    |> 
    List.fold_left (fun (lbl, acc, map) raw ->
      let line = String.trim raw in
      if line = "" || line.[0] = '#' 
        then (lbl, acc, map) (* empty line - skip *)
      else if line.[String.length line - 1] = ':' 
        then    
        (* label line, add current instructions to map and start over *)
        let label = Some (String.sub line 0 (String.length line - 1)) in
        (label, [], add_to_map lbl acc map)
      else (* instruction, add to current list of instructions *)
        (lbl, parse_instr line :: acc, map)
    ) 
    (None, [], LabelMap.empty)  (* starting point *)
    |>
    (* add the last block to the map and return *)
    fun (lbl, acc, map) -> add_to_map lbl acc map 

(* reads the file and returns a list of strings (one per line) *)
(* DO NOT CHANGE *)
let parse_file filename =
  let ic = open_in filename in
  let lines = read_lines ic in
  close_in ic;
  parse_lines lines


(* ── Drawing ───────EXTEND WITH MORE CASES ───────────────────────────────────────────── *)

let scale  = 5 (* DO NOT CHANGE *)
let canvas = 100 * scale (* DO NOT CHANGE *)

(* This function implements the drawing primitives. DO NOT CHANGE *)
let draw sv t = function
  | Square (c, x, y, w, h) ->
      let corners = [| screen sv t x y; screen sv t (x+w) y;
                       screen sv t (x+w) (y+h); screen sv t x (y+h) |] in
      let (sx, sy) = corners.(0) and (sx2, sy2) = corners.(2) in
      Printf.printf "square color=%s local=(%d,%d) %dx%d  screen=(%d,%d)-(%d,%d)\n%!"
        c x y w h sx sy sx2 sy2;
      set_color (color_of_string c);
      fill_poly corners
  | Line (c, lw, x1, y1, x2, y2) ->
      let (sx1, sy1) = screen sv t x1 y1 in
      let (sx2, sy2) = screen sv t x2 y2 in
      Printf.printf "line color=%s lw=%d local=(%d,%d)->(%d,%d)  screen=(%d,%d)->(%d,%d)\n%!"
        c lw x1 y1 x2 y2 sx1 sy1 sx2 sy2;
      set_color (color_of_string c);
      set_line_width (lw * sv);
      moveto sx1 sy1;
      lineto sx2 sy2
  | Image (file, x, y, sc, rot) ->
      let (px, py) = screen sv t x y in
      let total_rot = rot +. t.angle *. 180.0 /. Float.pi in
      let (src_w, src_h, buf) = Image.load_rgba file in
      let (out_w, out_h, img) = Image.render_image buf src_w src_h sc total_rot in
      Printf.printf "image %s at (%d,%d) scale=%.2f rot=%.1f -> %dx%d px\n%!"
        file px py sc total_rot out_w out_h;
      draw_image img px py
  | Text (c, x, y, size, txt) ->
      let (px, py) = screen sv t x y in
      Printf.printf "text %S at (%d,%d) size=%d\n%!" txt px py size;
      set_color (color_of_string c);
      set_font (Printf.sprintf "-*-times-medium-r-normal--%d-*-*-*-*-*-iso8859-1" size);
      moveto px py;
      draw_string txt

  |_ -> assert(false) (*adicionado por MIM porque o pattern matching nao era exaustivo*)

(* ── Interpreter ───────EXTEND WITH MORE CASES ─────────────────────────────────────── *)

let rec run program t label =
  match LabelMap.find_opt label program with
  | None        -> failwith ("undefined label: " ^ label)
  | Some instrs -> ignore (List.fold_left (exec_instr program) t instrs)

and exec_instr (program:program) t = function   (*o fix foi colocar program em vez de _*)
  (* EXTEND WITH THE NEW OPERATIONS, RAW DRAWING PRIMITIVES ARE REDIRECTED TO DRAW *)
  | Call label ->
    run program t label; t
     (*run program t label;t (*O t é o contexto atual do programa que tem ox, oy e angle*) (*Nao dá pra fazer o draw diretamente porque é string e nao é do tipo instructio
      nem da pra fazer draw scale t (parse_instr label)*)*)
  |Translate (dx, dy) ->
    _apply_translate t dx dy (*o aplly_translate devolve um t com as origens alteradas*)

  |Rotate deg ->
    _apply_rotate t deg (*aplica a rotaçao por deg graus*)

  |RepeatRotate (label, n, deg) -> 
    let rec repeat i curr_t =
      if i < n then begin
        run program curr_t label;
        repeat (i+1) (_apply_rotate curr_t deg)
      end
    in repeat 0 t; t 

  |RepeatPlanar (label, nx,ny,dx,dy,sy) ->
    let rec loop i j = (*n começa como 0*)
    if j>= ny then () (*tava a dar erro se devolvesse o t*) 
    else if i>= nx then loop 0 (j+1) (*volta pra primeira coluna*)
    else
      let deslocax = (float_of_int i *.dx) in 
      let deslocay = (float_of_int j *.dy ) +. (float_of_int i *. sy) in
      let t_atual = _apply_translate t deslocax deslocay in (*o aplly translate transforma o t*)
      run program t_atual label;
      loop (i+1) j
   (*Fim do loop quanto atingir as copias todas*)
    in loop 0 0; t
      

  |instr ->
      draw scale t instr; t


(* ── Entry point ────────DO NOT CHANGE ─────────────────────────────────────────────── *)

let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample1.pict"
  in
  let program = parse_file filename in
  open_graph (Printf.sprintf " %dx%d" canvas canvas);
  set_window_title filename;
  set_color white; fill_rect 0 0 canvas canvas;
  run program origin "main";
  ignore (wait_next_event [Key_pressed])
