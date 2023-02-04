open! Core
open! Async

module Stashed_changes : sig
  type t =
    | Stashed_hash of string
    | Nothing_stashed
end

module Commit_or_abort : sig
  type ('commit, 'abort) t =
    | Commit of
        { message : string
        ; data : 'commit
        }
    | Abort of 'abort
end

module Completion_status : sig
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

val update
  :  unit
  -> max_retries:int
  -> f:(unit -> ('commit, 'abort) Commit_or_abort.t Deferred.t)
  -> (('commit, 'abort) Completion_status.t, Error.t * Stashed_changes.t) Result.t
     Deferred.t
