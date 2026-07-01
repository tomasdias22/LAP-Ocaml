(* ── Image loading via ImageMagick ──────────────────────────────────────── *)

(* Returns (width, height, rgba_bytes) where rgba_bytes is packed RGBA,
   row 0 at the top (ImageMagick convention). *)
let load_rgba filename =
  let ic = Unix.open_process_in
    (Printf.sprintf "magick identify -format '%%wx%%h' %s" (Filename.quote filename)) in
  let dims = try input_line ic with End_of_file -> "0x0" in
  ignore (Unix.close_process_in ic);
  let (w, h) = Scanf.sscanf dims "%dx%d" (fun a b -> (a, b)) in
  let n = w * h * 4 in
  let buf = Bytes.create n in
  let ic = Unix.open_process_in
    (Printf.sprintf "magick %s -alpha on rgba:-" (Filename.quote filename)) in
  let rec fill pos =
    if pos >= n then ()
    else match input ic buf pos (n - pos) with
      | 0   -> ()
      | got -> fill (pos + got)
  in
  fill 0;
  ignore (Unix.close_process_in ic);
  (w, h, buf)

(* Sample a pixel at (src_x, src_y) where y=0 is the bottom.
   Returns Graphics.transp for out-of-bounds or low-alpha pixels. *)
let sample buf src_w src_h src_x src_y =
  if src_x < 0 || src_x >= src_w || src_y < 0 || src_y >= src_h then
    Graphics.transp
  else
    let row = src_h - 1 - src_y in
    let i   = (row * src_w + src_x) * 4 in
    let r = Char.code (Bytes.get buf  i)      in
    let g = Char.code (Bytes.get buf (i + 1)) in
    let b = Char.code (Bytes.get buf (i + 2)) in
    let a = Char.code (Bytes.get buf (i + 3)) in
    if a < 128 then Graphics.transp else Graphics.rgb r g b

(* Build a Graphics.image by applying inverse scale+rotation to sample the
   source. Returns (out_w, out_h, image). *)
let render_image buf src_w src_h scale rot_deg =
  let theta = rot_deg *. Float.pi /. 180.0 in
  let cos_t = cos theta and sin_t = sin theta in
  let sw = float_of_int src_w *. scale in
  let sh = float_of_int src_h *. scale in
  let out_w = int_of_float (ceil (Float.abs (sw *. cos_t) +. Float.abs (sh *. sin_t))) in
  let out_h = int_of_float (ceil (Float.abs (sw *. sin_t) +. Float.abs (sh *. cos_t))) in
  let ocx    = float_of_int out_w /. 2.0 in
  let ocy    = float_of_int out_h /. 2.0 in
  let src_cx = float_of_int src_w /. 2.0 in
  let src_cy = float_of_int src_h /. 2.0 in
  let pixels = Array.init out_h (fun row ->
    Array.init out_w (fun col ->
      let dx = float_of_int col -. ocx in
      let dy = ocy -. float_of_int row in
      let sdx =  dx *. cos_t +. dy *. sin_t in
      let sdy = -.dx *. sin_t +. dy *. cos_t in
      let src_x = int_of_float (Float.round (sdx /. scale +. src_cx)) in
      let src_y = int_of_float (Float.round (sdy /. scale +. src_cy)) in
      sample buf src_w src_h src_x src_y
    )
  ) in
  (out_w, out_h, Graphics.make_image pixels)
