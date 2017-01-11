(* Utility functions, used internally. *)

let url_encode s =
  let l = String.length s in
  let b = Buffer.create l in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char b c
    | _ ->
        let hex = "0123456789ABCDEF" in
        Buffer.add_char b '%';
        Buffer.add_char b hex.[Char.code c lsr 4];
        Buffer.add_char b hex.[Char.code c land 0xf]
  done;
  Buffer.contents b

let encode_form_string s =
  let l = String.length s in
  let b = Buffer.create l in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '*' ->
        Buffer.add_char b c
    | ' ' ->
        Buffer.add_char b '+'
    | _ ->
        let hex = "0123456789ABCDEF" in
        Buffer.add_char b '%';
        Buffer.add_char b hex.[Char.code c lsr 4];
        Buffer.add_char b hex.[Char.code c land 0xf]
  done;
  Buffer.contents b

let escape s =
  let b = Buffer.create 128 in
  String.iter
    (fun c ->
       match c with
         '\000'..'\031' | '\127'..'\255' ->
            Buffer.add_string b (Printf.sprintf "\\x%02X" (Char.code c))
       | _ ->
            Buffer.add_char b c)
    s;
  Buffer.contents b

type meth = [`GET | `POST | `PUT | `DELETE | `HEAD]

let string_of_meth m =
  match m with
    `GET    -> "GET"
  | `POST   -> "POST"
  | `PUT    -> "PUT"
  | `DELETE -> "DELETE"
  | `HEAD   -> "HEAD"

type request =
  { secure : bool;
    meth : meth;
    uri : string;
    query : (string * string) list;
    headers : (string * string) list;
    payload : string }

let request
      ?(secure=true) ~meth ~host ~uri
      ?(query = []) ?(headers = []) ?(payload = "") () =
  {secure; meth; uri; query; payload;
   headers = ("host", host) :: headers}

let print_curl_request {secure; meth; uri; query; headers; payload} =
  Format.eprintf "curl '%s://%s%s'"
    (if secure then "https" else "http") (List.assoc "host" headers) uri;
  if query <> [] then begin
    Format.eprintf "'?";
    List.iteri
      (fun i (f, v) ->
         if i <> 0 then Format.eprintf "&";
         Format.eprintf "%s=%s" (url_encode f) (url_encode v))
      query;
    Format.eprintf "'"
  end;
  List.iter (fun (k, v) -> Format.eprintf " -H '%s: %s'" k v)
    headers;
  begin match meth with
    `GET  -> ()
  | `POST -> Format.eprintf " --data-binary '%s'" (escape payload)
  | m -> Format.eprintf " -X %s" (string_of_meth m)
  end;
  Format.eprintf "@."

(****)

let to_ISO8601 ?(extended=false) t =
  let open Unix in
  let t = gmtime t in
  if extended then
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  else
    Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ"
      (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

(****)

module Debug = struct
  type t = { mutable state : bool; name : string; desc : string }

  let debugs = ref []
  let association = Hashtbl.create 11

  let make s desc l =
    let d =
      try
        List.assoc s !debugs
      with Not_found ->
        let d = { state = false; name = s; desc = desc } in
        debugs := (s, d) :: !debugs;
        d
    in
    List.iter (fun s' -> Hashtbl.add association s' s) l;
    fun () -> d.state

  let print () =
    Format.eprintf "Debug options:@.";
    List.iter
      (fun (_, d) -> Format.eprintf "    %s: %s@." d.name d.desc) !debugs;
    exit 1

  let rec enable s =
    if s = "help" || not (List.mem_assoc s !debugs) then
      print ()
    else
      try
        let d = List.assoc s !debugs in
        if not d.state then begin
          d.state <- true;
          List.iter enable (Hashtbl.find_all association s)
        end
      with Not_found -> ()

  let all = make "all" "Enable all debugging options." []
end

(****)

module Xml = struct
  let text i =
    match Xmlm.peek i with
    | `Data d -> ignore (Xmlm.input i); d
    | `El_end -> ""
    | _       -> assert false


  let repeat f i =
    let rec repeat_rec f i acc =
      match Xmlm.peek i with
        `El_end -> List.rev acc
      | _       -> repeat_rec f i (f i :: acc)
    in
    repeat_rec f i []

  let element_end i =
    match Xmlm.input i with
    | `El_end -> ()
    | _       -> assert false

  let element f i =
    match Xmlm.input i with
    | `El_start ((_, nm), _) ->
      let x = f i in
      element_end i;
      (nm, x)
    | _ ->
      assert false

  let field i = element text i

  let record = repeat field
end

module Param = struct
  let string k v rem =
    match v with
      Some v -> (k, v) :: rem
    | _      -> rem

  let int k v rem =
    match v with
      Some v -> (k, string_of_int v) :: rem
    | _      -> rem

  let bool k v rem =
    match v with
      Some true -> (k, "true") :: rem
    | _         -> rem

  let custom k f v rem =
    match v with
      Some v -> (k, f v) :: rem
    | _      -> rem
end
