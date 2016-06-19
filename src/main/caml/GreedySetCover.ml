include DisableGenericCompare

module type SET = sig
  type t
  val cardinal : t -> int
  val diff : t -> t -> t
end

module Make (S : SET) = struct
  let solve ls = begin
    let sets = Array.of_list ls in
    let rec pick_one_greedily i k n best best_magnitude =
      if i = n then
        best, k
      else begin
        let el = Array.get sets i in
        let magnitude = S.cardinal el in
        assert (magnitude >= 0);
        if magnitude = 0 then
          pick_one_greedily (i+1) k n best best_magnitude
        else begin
          Array.set sets k el;
          let better = magnitude > best_magnitude in
          pick_one_greedily (i + 1) (k + 1) n
            (if better then k         else best)
            (if better then magnitude else best_magnitude)
        end
      end
    in
    let rec solve_step solution_rev n =
      let best, n = pick_one_greedily 0 0 n ~-1 0 in
      if n = 0 then
        List.rev solution_rev
      else begin
        let best_element = Array.get sets best in
        if 0 = S.cardinal best_element then
          solve_step solution_rev 0
        else begin
          let solution_rev = best_element::solution_rev in
          ArrayUtil.fold_lefti_sub
            (fun i _ elt -> Array.set sets i (S.diff elt best_element))
            ()
            sets 0 n;
          solve_step solution_rev n
        end
      end
    in
    solve_step [] (Array.length sets)
  end
end
