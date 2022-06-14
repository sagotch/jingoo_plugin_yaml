open Jingoo

let rec yaml_of_value = function
  | `Null -> Jg_types.Tnull
  | `Bool b -> Jg_types.Tbool b
  | `Float f -> Jg_types.Tfloat f
  | `String s -> Jg_types.Tstr s
  | `A a -> Jg_types.Tlist (List.map yaml_of_value a)
  | `O o -> Jg_types.Tobj (List.map (fun (k, v) -> (k, yaml_of_value v)) o)

let yaml_of_string = function
  | Jg_types.(Tstr s | Tsafe s) ->
    begin match Yaml.of_string s with
      | Ok y -> yaml_of_value y
      | Error (`Msg e) -> failwith e
    end
  | x -> Jg_types.failwith_type_error_1 "yaml_of_string" x

let () =
  Jg_stub.add_func ~namespace:"YAML" ~func_name:"of_string" (Jg_types.func_arg1_no_kw yaml_of_string)

let rec yaml_to_value = function
  | Jg_types.Tint i -> `Float (float_of_int i)
  | Tfloat f -> `Float f
  | Tstr s | Tsafe s -> `String s
  | Tbool b -> `Bool b
  | Tobj o -> `O (List.map (fun (k, v) -> (k, yaml_to_value v)) o)
  | Thash o -> `O (Hashtbl.fold (fun k v acc -> (k, yaml_to_value v) :: acc) o [])
  | Tlist l | Tset l -> `A (List.map yaml_to_value l)
  | Tpat _ | Tfun _ as x -> Jg_types.failwith_type_error_1 "yaml_to_value" x
  | Tnull -> `Null
  | Tarray a -> `A (Array.fold_right (fun x acc -> yaml_to_value x :: acc) a [])
  | Tlazy l -> yaml_to_value (Lazy.force l)
  | Tvolatile f -> yaml_to_value (f ())

let yaml_to_string x =
  match yaml_to_value x |> Yaml.to_string with
  | Ok s -> Jg_types.Tstr s
  | Error (`Msg e) -> failwith e

let () =
  Jg_stub.add_func ~namespace:"YAML" ~func_name:"to_string" (Jg_types.func_arg1_no_kw yaml_to_string)

(* Copyright 2022 - Julien Sagot <contact@sagotch.fr>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)
