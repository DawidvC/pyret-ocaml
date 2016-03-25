(** Library for creating test suites
    (allows suites to be exposed while still hiding module information) *)

(* This module is an extension of OUnit2, so we include its signature *)
include module type of struct include OUnit2 end

type printer = string -> unit

(** A Test Suite definition set *)
module type TESTS = sig
  (** The name of the test suite. If not None,
      a string saying "Running <suite_name> Tests..." will
      be printed to STDOUT when this suite is run *)
  val suite_name : string option
  (** The list of tests to run *)
  val suite : OUnit2.test
end

(** A Suite of tests which can be run *)
module type SUITE = sig
  (** Runs this suite's tests, printing the informational string
      in {!TESTS.suite_name} to `printer', if applicable *)
  val run_tests : printer -> unit
end

(** Constructs a test suite suitable for running from
    the given test definition module. *)
module MakeSuite : functor (Defs : TESTS) -> SUITE
