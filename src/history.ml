open! Core

type t =
  { past : int list
  ; future : int list
  }
[@@deriving sexp]

let empty = { past = []; future = [] }

(* Adding a token appends it to the [past] but now the [future] is no longer written. In the case
   where the future move is the same as the move, then simply move forward in the history. *)
let place_piece t ~column_idx =
  match t.future with
  | next_column_idx :: old_future when next_column_idx = column_idx ->
    { past = column_idx :: t.past; future = old_future }
  | (_ : int list) -> { past = column_idx :: t.past; future = [] }
;;

module Update_result = struct
  type nonrec t =
    { updated_history : t
    ; column_idx : int
    }
end

let undo t =
  match t.past with
  | [] -> error_s [%message "No more history to undo"]
  | latest_column_idx :: old_past ->
    let updated_history = { past = old_past; future = latest_column_idx :: t.future } in
    Ok { Update_result.updated_history; column_idx = latest_column_idx }
;;

let next_move t =
  match t.future with
  | [] -> error_s [%message "No next move in history"]
  | next_column_idx :: (_ : int list) -> Ok next_column_idx
;;

let rec undo_until_start t =
  match t.past with
  | [] -> t
  | last_column_idx :: past ->
    undo_until_start { past; future = last_column_idx :: t.future }
;;
