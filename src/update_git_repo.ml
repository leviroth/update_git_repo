open! Core
open! Async

module Git = struct
  module Process = struct
    (* We shadow the Process module to ensure that [working_dir] is always
       passed. *)

    let run ~working_dir = Process.run ~working_dir
    let run_exn ~working_dir = Process.run_exn ~working_dir
  end

  let prog = "git"

  let run_ignoring_output_exn ~prog ~args ~working_dir () =
    let%bind (_ : string) = Process.run_exn ~working_dir () ~prog ~args in
    return ()
  ;;

  let status_is_clean () ~working_dir =
    let%bind status =
      Process.run_exn ~working_dir () ~prog ~args:[ "status"; "--porcelain" ]
    in
    return (String.is_empty status)
  ;;

  let stash () ~message ~working_dir =
    run_ignoring_output_exn
      ~working_dir
      ()
      ~prog
      ~args:[ "stash"; "push"; "--include-untracked"; "--message"; message ]
  ;;

  let get_hash () ~refspec ~working_dir =
    Process.run_exn ~working_dir () ~prog ~args:[ "rev-parse"; refspec ] >>| String.rstrip
  ;;

  let get_last_stash_hash () ~working_dir = get_hash () ~refspec:"stash@{0}" ~working_dir

  let fetch () ~working_dir =
    run_ignoring_output_exn ~working_dir () ~prog ~args:[ "fetch"; "origin"; "main" ]
  ;;

  let reset_to_upstream () ~working_dir =
    run_ignoring_output_exn () ~prog ~args:[ "reset"; "--hard"; "@{u}" ] ~working_dir
  ;;

  let add_all () ~working_dir =
    run_ignoring_output_exn () ~prog ~args:[ "add"; "." ] ~working_dir
  ;;

  let commit () ~message ~working_dir =
    let%bind () =
      run_ignoring_output_exn () ~prog ~args:[ "commit"; "-m"; message ] ~working_dir
    in
    get_hash () ~refspec:"HEAD" ~working_dir
  ;;

  let push () ~working_dir =
    match%bind Process.run ~working_dir () ~prog ~args:[ "push" ] with
    | Ok (_ : string) -> return (Ok ())
    | Error _ as error -> return error
  ;;
end

module Stashed_changes = struct
  type t =
    | Stashed_hash of string
    | Nothing_stashed
end

let stash_if_necessary ~message ~working_dir =
  match%bind Git.status_is_clean () ~working_dir with
  | true -> return Stashed_changes.Nothing_stashed
  | false ->
    let%bind () = Git.stash () ~message ~working_dir in
    let%bind hash = Git.get_last_stash_hash () ~working_dir in
    return (Stashed_changes.Stashed_hash hash)
;;

let fetch_and_reset () ~working_dir =
  let%bind () = Git.fetch () ~working_dir in
  Git.reset_to_upstream () ~working_dir
;;

module Commit_or_abort = struct
  type ('commit, 'abort) t =
    | Commit of
        { message : string
        ; data : 'commit
        }
    | Abort of 'abort
end

module Completion_status = struct
  type ('committed, 'aborted) t =
    | Committed of
        { message : string
        ; hash : string
        ; data : 'committed
        }
    | Aborted of
        { stash : Stashed_changes.t
        ; data : 'aborted
        }
end

(* TODO: specify remote, branch *)
let update () ~directory:working_dir ~max_retries ~f =
  (match Int.is_negative max_retries with
  | false -> ()
  | true -> raise_s [%message "[max_retries] cannot be negative" (max_retries : int)]);
  let%bind () = fetch_and_reset () ~working_dir in
  Deferred.repeat_until_finished max_retries (fun max_retries ->
      match%bind f () with
      | Commit_or_abort.Abort data ->
        let%bind stash =
          stash_if_necessary ~message:"Changes present when aborting update" ~working_dir
        in
        return (`Finished (Ok (Completion_status.Aborted { stash; data })))
      | Commit { message; data } ->
        let%bind () = Git.add_all () ~working_dir in
        let%bind hash = Git.commit () ~message ~working_dir in
        (match%bind Git.push () ~working_dir with
        | Ok () ->
          return (`Finished (Ok (Completion_status.Committed { message; hash; data })))
        | Error error ->
          (match max_retries > 0 with
          | true ->
            let%bind () = fetch_and_reset () ~working_dir in
            return (`Repeat (max_retries - 1))
          | false ->
            let%bind failure_stash_hash =
              stash_if_necessary
                ~message:"Changes present when update failed"
                ~working_dir
            in
            return
              (`Finished
                (Error
                   ( Error.tag error ~tag:"Failed; last error is attached."
                   , failure_stash_hash ))))))
;;
