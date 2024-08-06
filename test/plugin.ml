open Ast_helper	
open Ast_invariants	
open Ast_iterator	
open Ast_mapper	
open Asttypes	
open Attr_helper	
open Builtin_attributes	
open CamlinternalMenhirLib	
open Depend	
open Docstrings	
open Lexer	
open Location	
open Longident	
open Parse	
open Parser	
open Parsetree	
open Pprintast	
open Printast	
open Syntaxerr	
(* open Unit_info	*)
open Arg_helper	
open Binutils	
open Build_path_prefix_map	
open Ccomp	
open Clflags	
(* open Compression	 *)
(* open Config_boot	 *)
(* open Config_main	 *)
open Config	
open Consistbl	
open Diffing	
open Diffing_with_keys	
open Domainstate	
open Identifiable	
open Int_replace_polymorphic_compare	
open Lazy_backtrack	
open Load_path	
open Local_store	
open Misc	
open Numbers	
open Profile	
open Strongly_connected_components	
open Targetint	
open Terminfo

open Warnings	
open Pparse	

module type AstSig = module type of Ast_helper
 [@@deriving show]

module type PparseSig = module type of Pparse
module ShowMyModuleSig : AstSig = Ast_helper
module Show1 : PparseSig = Pparse
