(*
  Copyright 2014 Google, Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 *)

include DisableGenericCompare

let sprintf = Printf.sprintf

let _ = Printexc.register_printer
  (fun e -> match e with
    | AnnotationChecker.Misplaced_annotation (p, s) ->
      Some ("Misplaced_annotation (" ^ (SourcePosition.to_string p)
            ^ " : " ^ s ^ ")")
    | AnnotationChecker.Parameter_value (p, s) ->
      Some ("Parameter_value (" ^ (SourcePosition.to_string p)
            ^ " : " ^ s ^ ")")
    | AnnotationChecker.Var_masked (p, n, q) ->
      Some ("Var_masked (" ^ (SourcePosition.to_string p)
            ^ " : " ^ (Identifier.to_string (Var.Name.as_id n))
            ^ " @ " ^ (SourcePosition.to_string q) ^ ")")
    | AnnotationChecker.Var_out_of_scope (p, n) ->
      Some ("Var_out_of_scope (" ^ (SourcePosition.to_string p)
            ^ " : " ^ (Identifier.to_string (Var.Name.as_id n)) ^ ")")
    | AnnotationChecker.Var_assign_after_use (p, n, q) ->
      Some ("Var_assign_after_use (" ^ (SourcePosition.to_string p)
            ^ " : " ^ (Identifier.to_string (Var.Name.as_id n))
            ^ " @ " ^ (SourcePosition.to_string q) ^ ")")
    | AnnotationChecker.Var_use_before_assign (p, n, d, b) ->
      Some (sprintf
              "Var_use_before_assign: %s: %s from %s not assigned at %s"
              (SourcePosition.to_string p)
              (Identifier.to_string (Var.Name.as_id n))
              (SourcePosition.to_string d)
              (SourcePosition.to_string b))
    | Failures.Ambiguous_production (p, id, q) ->
      Some ("Ambiguous_production (" ^ (SourcePosition.to_string p)
            ^ " " ^ (Identifier.to_string id)
            ^ " " ^ (SourcePosition.to_string q) ^ ")")
    | Failures.Bad_syntax (p, s) ->
      Some ("Bad_syntax (" ^ (SourcePosition.to_string p) ^ " : " ^ s ^ ")")
    | Failures.Limit_not_regular (p, s) ->
      Some ("Limit_not_regular (" ^ (SourcePosition.to_string p)
            ^ " : " ^ s ^ ")")
    | Failures.No_such_production (p, id) ->
      Some ("No_such_production (" ^ (SourcePosition.to_string p) ^ " : "
            ^ (Identifier.to_string id) ^ ")")
    | Failures.Undeclared_symbol (p, syms, dp) ->
      Some (Printf.sprintf "Undeclared_symbol (%s, %s, %s)"
              (SourcePosition.to_string p)
              (Stringer.s Var.Symbols.stringer syms)
              (SourcePosition.to_string dp))
    | PegToIL.Indirect_left_recursion (chain) ->
      Some ("Indirect_left_recursion ("
            ^ (Stringer.s (Stringer.list Label.stringer) chain)
            ^ ")")
    | ScalarCharValue.No_digit p ->
      Some ("No_digit (" ^ (SourcePosition.to_string p) ^ ")")
    | ScalarCharValue.Not_a_scalar_value_node p ->
      Some ("Not_a_scalar_value_node (" ^ (SourcePosition.to_string p) ^ ")")
    | Unpack.Missing_config (ls, config, missing) ->
      Some (sprintf "Missing %s to match (%s) at path %s"
              (Stringer.s (Stringer.list Stringer.string) missing)
              (Stringer.s (fun out _ -> config out) ())
              (Stringer.s (Stringer.list Stringer.string) ls))
    | Unpack.Illegal_config (ls, config, enc) ->
      Some (sprintf "Mismatch between (%s) at path %s and (%s)"
              (Stringer.s (fun out _ -> config out) ())
              (Stringer.s (Stringer.list Stringer.string) ls)
              (Stringer.s Encodable.json_stringer enc))
    | Unpack.Unused_config  (ls, config, unused) ->
      Some (sprintf "Configuration (%s) at path %s cannot use %s"
              (Stringer.s (fun out _ -> config out) ())
              (Stringer.s (Stringer.list Stringer.string) ls)
              (Stringer.s (Stringer.list Stringer.string) unused))
    | _ -> None)
