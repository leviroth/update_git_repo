open! Core
open! Async

module Git = struct
  let prog = "git"

  let run_ignoring_output_exn ~prog ~args () =
    let%bind (_ : string) = Process.run_exn ~prog ~args () in
    return ()
  ;;

  let status_is_clean () =
    let%bind status = Process.run_exn ~prog ~args:[ "status"; "--porcelain" ] () in
    return (String.is_empty status)
  ;;

  let stash ~message =
    run_ignoring_output_exn
      ~prog
      ~args:[ "stash"; "push"; "--include-untracked"; "--message"; message ]
      ()
  ;;

  let get_hash ~refspec =
    Process.run_exn ~prog ~args:[ "rev-parse"; refspec ] () >>| String.rstrip
  ;;

  let get_last_stash_hash () = get_hash ~refspec:"stash@{0}"
  let fetch () = run_ignoring_output_exn ~prog ~args:[ "fetch"; "origin"; "main" ] ()

  let reset_to_upstream () =
    run_ignoring_output_exn ~prog ~args:[ "reset"; "--hard"; "@{u}" ] ()
  ;;

  let add_all () = run_ignoring_output_exn ~prog ~args:[ "add"; "." ] ()

  let commit ~message =
    let%bind () = run_ignoring_output_exn ~prog ~args:[ "commit"; "-m"; message ] () in
    get_hash ~refspec:"HEAD"
  ;;

  let push () =
    match%bind Process.run ~prog ~args:[ "push" ] () with
    | Ok (_ : string) -> return (Ok ())
    | Error _ as error -> return error
  ;;
end

module Stashed_changes = struct
  type t =
    | Stashed_hash of string
    | Nothing_stashed
end

let stash_if_necessary ~message =
  match%bind Git.status_is_clean () with
  | true -> return Stashed_changes.Nothing_stashed
  | false ->
    let%bind () = Git.stash ~message in
    let%bind hash = Git.get_last_stash_hash () in
    return (Stashed_changes.Stashed_hash hash)
;;

let fetch_and_reset () =
  let%bind () = Git.fetch () in
  Git.reset_to_upstream ()
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
let update () ~max_retries ~f =
  (match Int.is_negative max_retries with
  | false -> ()
  | true -> raise_s [%message "[max_retries] cannot be negative" (max_retries : int)]);
  let%bind () = fetch_and_reset () in
  Deferred.repeat_until_finished max_retries (fun max_retries ->
      match%bind f () with
      | Commit_or_abort.Abort data ->
        let%bind stash =
          stash_if_necessary ~message:"Changes present when aborting update"
        in
        return (`Finished (Ok (Completion_status.Aborted { stash; data })))
      | Commit { message; data } ->
        let%bind () = Git.add_all () in
        let%bind hash = Git.commit ~message in
        (match%bind Git.push () with
        | Ok () ->
          return (`Finished (Ok (Completion_status.Committed { message; hash; data })))
        | Error error ->
          (match max_retries > 0 with
          | true ->
            let%bind () = fetch_and_reset () in
            return (`Repeat (max_retries - 1))
          | false ->
            let%bind failure_stash_hash =
              stash_if_necessary ~message:"Changes present when update failed"
            in
            return
              (`Finished
                (Error
                   ( Error.tag error ~tag:"Failed; last error is attached."
                   , failure_stash_hash ))))))
;;
