module E2I : EncToIL.S with module R = FileTestSuite.Reporting

val base_dirs : FileTestSuite.TestDirs.t

type enc_test_info = {
  test_name      : string;
  test_dirs      : FileTestSuite.TestDirs.t;
  orig_grammar   : FileTestSuite.Reporting.meta_t Grammar.grammar;
  simple_grammar : FileTestSuite.Reporting.meta_t Grammar.grammar;
  gen            : FileTestSuite.Reporting.meta_t CodeGenerator.t;
  bundle         : FileTestSuite.Reporting.meta_t CodeGenerator.GrammarBundle.t;
  tool_set       : FileTestSuite.Reporting.meta_t CodeGenerator.ToolSet.t;
  opts           : CodeGenerator.Opts.t;
  enc            : FileTestSuite.Reporting.meta_t Enc.t;
  str_enc        : FileTestSuite.Reporting.meta_t Enc.t;
  (** Like enc, but only encodes strings to test pruning. *)
}

type enc_runner_opts = {
  tool_kinds : ToolKind.Set.t
}

type enc_test_opts = {
  encode_strings_only : bool;
}

module EncTestInfo : FileTestSuite.TestInfo
  with type t = enc_test_info
  and type test_opts = enc_test_opts
  and type runner_opts = enc_runner_opts

module EncFileTestSuite : FileTestSuite.S with module TI = EncTestInfo

val test_debug_hooks : E2I.DebugHooks.t

val data_grammar_to_string :
     E2I.DebugHooks.Data.t
  -> ((E2I.R.meta_t * E2I.DebugHooks.NodeId.t * E2I.DebugHooks.Data.t option)
         Grammar.grammar)
  -> string

val regex_grammar_to_string :
  gen:bool
  -> ((E2I.R.meta_t * E2I.DebugHooks.NodeId.t * E2I.DebugHooks.token option
       * E2I.DebugHooks.token option)
         Grammar.grammar)
  -> string
