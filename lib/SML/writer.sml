(* ************************************************************************* *\
 
   Generic libraries of the ASM Workbench:
   Writer (serialisation)
 
   Developed by G. Del Castillo (1997-2000, 2022)

   *************************************************************************

   structure Write :Write_sig
   -- generic serialiser for simple data types, lists, tuples, etc.
      (output is text, specifically in SML / ASM-SL syntax)

   structure StringBasicWriter :BasicWriter_sig
   structure FileBasicWriter :BasicWriter_sig
   -- basic mechanisms to output to file or string
  
   structure Output :Output_sig
   -- wrappers for serialisation terms ('writers') to actually do the output

\* ************************************************************************* *)

signature Write_sig =
sig
  type ('a, 'strm) BASIC_WRITER
  type ('b, 'strm) WRITER

  val parseFormatString : string -> string list

  val literal : (string, 'strm) BASIC_WRITER -> (string, 'strm) WRITER

  val const : ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
              'a ->
              (string, 'strm) BASIC_WRITER ->
              ('a, 'strm) WRITER

  val char  : (string, 'strm) BASIC_WRITER -> (char, 'strm) WRITER
                                                            
  val char_ : (char -> string) ->      (* user-defined conversion function *)
              (string, 'strm) BASIC_WRITER -> (char, 'strm) WRITER
                                                            
  val unit  : (string, 'strm) BASIC_WRITER -> (unit, 'strm) WRITER
                                                            
  val unit_ : (unit -> string) ->      (* user-defined conversion function *)
              (string, 'strm) BASIC_WRITER -> (unit, 'strm) WRITER

  val bool  : (string, 'strm) BASIC_WRITER -> (bool, 'strm) WRITER
                                                            
  val bool_ : (bool -> string) ->      (* user-defined conversion function *)
              (string, 'strm) BASIC_WRITER -> (bool, 'strm) WRITER

  val int   : (string, 'strm) BASIC_WRITER -> (int, 'strm) WRITER
                                                           
  val int_  : (int -> string) ->       (* user-defined conversion function *)
              (string, 'strm) BASIC_WRITER -> (int, 'strm) WRITER

  val real  : (string, 'strm) BASIC_WRITER -> (real, 'strm) WRITER

  val real_ : (real -> string) ->      (* user-defined conversion function *)
              (string, 'strm) BASIC_WRITER -> (real, 'strm) WRITER

  val string  : (string, 'strm) BASIC_WRITER -> (string, 'strm) WRITER

  val string_ : (string -> string) ->  (* user-defined conversion function *)
                (string, 'strm) BASIC_WRITER -> (string, 'strm) WRITER

  val option : ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
               (string, 'strm) BASIC_WRITER -> ('a option, 'strm) WRITER

  val option_ : (string * string) ->       (* format string pair, e.g. ("Some($1)", "None") *)
                ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
                (string, 'strm) BASIC_WRITER -> ('a option, 'strm) WRITER

  val list  : ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
              (string, 'strm) BASIC_WRITER -> ('a list, 'strm) WRITER

  val list_ : string -> string ->      (* format strings, e.g. "[ $$ ]" "$1, $2" or "$$\n" "$1\n" *)
              ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
              (string, 'strm) BASIC_WRITER -> ('a list, 'strm) WRITER

  val empty : (string,'strm) BASIC_WRITER -> ('a, 'strm) WRITER

  val single : ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
               (string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER

  val single_ : string ->                (* format string, e.g. "value is $1" *)
                ((string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) ->
                (string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER

  val pair : ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
               ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) ) ->
             (string, 'strm) BASIC_WRITER -> ('a1 * 'a2, 'strm) WRITER

  val pair_ : string ->                (* format string, e.g. "< first = $1, second = $2 >" *)
              ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) ) ->
              (string, 'strm) BASIC_WRITER -> ('a1 * 'a2, 'strm) WRITER
                                                                 
  val triple : ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) ) ->
               (string, 'strm) BASIC_WRITER -> ('a1 * 'a2 * 'a3, 'strm) WRITER

  val triple_ : string ->                (* format string, e.g. "< e1 = $1, e2 = $2, e3 = $3 >" *)
                ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER)) ->
                (string, 'strm) BASIC_WRITER -> ('a1 * 'a2 * 'a3, 'strm) WRITER

  val tuple4 : ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a4, 'strm) WRITER) ) ->
               (string, 'strm) BASIC_WRITER -> ('a1 * 'a2 * 'a3 * 'a4, 'strm) WRITER

  val tuple4_ : string ->                (* format string, e.g. "< $1, $2, $3, $4 >" *)
                ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a4, 'strm) WRITER) ) ->
                (string, 'strm) BASIC_WRITER -> ('a1 * 'a2 * 'a3 * 'a4, 'strm) WRITER

  val tuple5 : ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a4, 'strm) WRITER) *
                 ((string, 'strm) BASIC_WRITER -> ('a5, 'strm) WRITER) ) ->
               (string, 'strm) BASIC_WRITER -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5, 'strm) WRITER

  val tuple5_ : string ->                (* format string, e.g. "($1,$2,$3,$4,$5)>" *)
                ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a4, 'strm) WRITER) *
                  ((string, 'strm) BASIC_WRITER -> ('a5, 'strm) WRITER) ) ->
                (string, 'strm) BASIC_WRITER -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5, 'strm) WRITER

  (* infix 5 ++ *)
  val ++ : ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
             ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) ) ->
           (string, 'strm) BASIC_WRITER -> ('a1 * 'a2, 'strm) WRITER

  val replace0 : (string, 'strm) BASIC_WRITER ->
                 ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) ) ->
                 'a1 -> string -> 'strm -> unit

  val replace1 : (string, 'strm) BASIC_WRITER ->
                 ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) ) ->
                 'a1 -> string -> 'strm -> unit

  val replace2 : (string, 'strm) BASIC_WRITER ->
                 ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) ) ->
                 ('a1 * 'a2) -> string -> 'strm -> unit

  val replace4 : (string, 'strm) BASIC_WRITER ->
                 ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) * 
                   ((string, 'strm) BASIC_WRITER -> ('a4, 'strm) WRITER) ) ->
                 ('a1 * 'a2 * 'a3 * 'a4) -> string -> 'strm -> unit

  val replace5 : (string, 'strm) BASIC_WRITER ->
                 ( ((string, 'strm) BASIC_WRITER -> ('a1, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a2, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a3, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a4, 'strm) WRITER) *
                   ((string, 'strm) BASIC_WRITER -> ('a5, 'strm) WRITER) ) ->
                 ('a1 * 'a2 * 'a3 * 'a4 * 'a5) -> string -> 'strm -> unit

  val datatype1 : ('a -> (string * 'a1 * ((string,'strm) BASIC_WRITER -> (('a1, 'strm) WRITER)))) ->
                  (string,'strm) BASIC_WRITER -> ('a, 'strm) WRITER

  val datatype1_ : ('a -> (string * 'a1 * (((string, 'strm) BASIC_WRITER) -> 'writers1 -> 'a1 -> string -> 'strm -> unit) * 'writers1)) ->
                   ((string, 'strm) BASIC_WRITER) -> 'a -> 'strm -> unit

  val datatype2 : ( ('a -> (string * 'a1 * ((string,'strm) BASIC_WRITER -> (('a1, 'strm) WRITER)))) * 
                    ('a -> (string * 'a2 * ((string,'strm) BASIC_WRITER -> (('a2, 'strm) WRITER)))) ) ->
                  (string,'strm) BASIC_WRITER -> ('a, 'strm) WRITER
                                                             
  val datatype2_ : ( ('a -> (string * 'a1 * (((string, 'strm) BASIC_WRITER) -> 'writers1 -> 'a1 -> string -> 'strm -> unit) * 'writers1)) *
                     ('a -> (string * 'a2 * (((string, 'strm) BASIC_WRITER) -> 'writers2 -> 'a2 -> string -> 'strm -> unit) * 'writers2)) ) ->
                   ((string, 'strm) BASIC_WRITER) -> 'a -> 'strm -> unit
                                                                      
  val datatype3 : ( ('a -> (string * 'a1 * ((string,'strm) BASIC_WRITER -> (('a1, 'strm) WRITER)))) * 
                    ('a -> (string * 'a2 * ((string,'strm) BASIC_WRITER -> (('a2, 'strm) WRITER)))) * 
                    ('a -> (string * 'a3 * ((string,'strm) BASIC_WRITER -> (('a3, 'strm) WRITER)))) ) ->
                  (string,'strm) BASIC_WRITER -> ('a, 'strm) WRITER
                                                             
  val datatype3_ : ( ('a -> (string * 'a1 * (((string, 'strm) BASIC_WRITER) -> 'writers1 -> 'a1 -> string -> 'strm -> unit) * 'writers1)) *
                     ('a -> (string * 'a2 * (((string, 'strm) BASIC_WRITER) -> 'writers2 -> 'a2 -> string -> 'strm -> unit) * 'writers2)) *
                     ('a -> (string * 'a3 * (((string, 'strm) BASIC_WRITER) -> 'writers3 -> 'a3 -> string -> 'strm -> unit) * 'writers3)) ) ->
                   ((string, 'strm) BASIC_WRITER) -> 'a -> 'strm -> unit
                                                                      
  val datatype4 : ( ('a -> (string * 'a1 * ((string,'strm) BASIC_WRITER -> (('a1, 'strm) WRITER)))) * 
                    ('a -> (string * 'a2 * ((string,'strm) BASIC_WRITER -> (('a2, 'strm) WRITER)))) * 
                    ('a -> (string * 'a3 * ((string,'strm) BASIC_WRITER -> (('a3, 'strm) WRITER)))) * 
                    ('a -> (string * 'a4 * ((string,'strm) BASIC_WRITER -> (('a4, 'strm) WRITER)))) ) ->
                  (string,'strm) BASIC_WRITER -> ('a, 'strm) WRITER
                                                             
  val datatype4_ : ( ('a -> (string * 'a1 * (((string, 'strm) BASIC_WRITER) -> 'writers1 -> 'a1 -> string -> 'strm -> unit) * 'writers1)) *
                     ('a -> (string * 'a2 * (((string, 'strm) BASIC_WRITER) -> 'writers2 -> 'a2 -> string -> 'strm -> unit) * 'writers2)) *
                     ('a -> (string * 'a3 * (((string, 'strm) BASIC_WRITER) -> 'writers3 -> 'a3 -> string -> 'strm -> unit) * 'writers3)) *
                     ('a -> (string * 'a4 * (((string, 'strm) BASIC_WRITER) -> 'writers4 -> 'a4 -> string -> 'strm -> unit) * 'writers4)) ) ->
                   ((string, 'strm) BASIC_WRITER) -> 'a -> 'strm -> unit
                                                                      
  val datatype5 : ( ('a -> (string * 'a1 * ((string,'strm) BASIC_WRITER -> (('a1, 'strm) WRITER)))) * 
                    ('a -> (string * 'a2 * ((string,'strm) BASIC_WRITER -> (('a2, 'strm) WRITER)))) * 
                    ('a -> (string * 'a3 * ((string,'strm) BASIC_WRITER -> (('a3, 'strm) WRITER)))) * 
                    ('a -> (string * 'a4 * ((string,'strm) BASIC_WRITER -> (('a4, 'strm) WRITER)))) * 
                    ('a -> (string * 'a5 * ((string,'strm) BASIC_WRITER -> (('a5, 'strm) WRITER)))) ) ->
                  (string,'strm) BASIC_WRITER -> ('a, 'strm) WRITER
                                                             
  val datatype5_ : ( ('a -> (string * 'a1 * (((string, 'strm) BASIC_WRITER) -> 'writers1 -> 'a1 -> string -> 'strm -> unit) * 'writers1)) *
                     ('a -> (string * 'a2 * (((string, 'strm) BASIC_WRITER) -> 'writers2 -> 'a2 -> string -> 'strm -> unit) * 'writers2)) *
                     ('a -> (string * 'a3 * (((string, 'strm) BASIC_WRITER) -> 'writers3 -> 'a3 -> string -> 'strm -> unit) * 'writers3)) *
                     ('a -> (string * 'a4 * (((string, 'strm) BASIC_WRITER) -> 'writers4 -> 'a4 -> string -> 'strm -> unit) * 'writers4)) *
                     ('a -> (string * 'a5 * (((string, 'strm) BASIC_WRITER) -> 'writers5 -> 'a5 -> string -> 'strm -> unit) * 'writers5)) ) ->
                   ((string, 'strm) BASIC_WRITER) -> 'a -> 'strm -> unit
end

(* ******************************************************************* *)

structure Write (* : Write_sig *) =
struct
  type ('a, 'strm) BASIC_WRITER = 'a -> 'strm -> unit
  type ('b, 'strm) WRITER       = 'b -> 'strm -> unit

  fun parseFormatString s =
      let val len = size s
          fun pfs (result, i, flag) =
            if i >= len
            then len :: result
            else if i = len - 1
            then pfs (i :: result, i + 1, true)
            else if String.sub (s, i) = #"$"
                    andalso (let val c = String.sub (s, i + 1) in Char.isDigit c orelse c = #"$" end)
            then pfs (i :: result, i + 2, true)
            else pfs (if flag then i :: result else result, i + 1, false)
          fun return (final, x :: (rest as prec_x :: _)) = return (String.substring (s, prec_x, x - prec_x) :: final, rest)
            | return (final, _) = final
      in return ([], pfs ([], 0, true))
      end

  fun literal (basic_writer :(string, 'strm) BASIC_WRITER) :(string, 'strm) WRITER =
    fn s => fn ostr => basic_writer s ostr

  fun const (writer :(string, 'strm) BASIC_WRITER -> ('a, 'strm) WRITER) (a :'a) (basic_writer :(string, 'strm) BASIC_WRITER) =
    fn _ => writer basic_writer a 
  
  fun char (basic_writer :(string, 'strm) BASIC_WRITER) :(char, 'strm) WRITER =
    fn c => basic_writer ("#\"" ^ (Char.toString c) ^ "\"")

  fun char_ char_cvt (basic_writer :(string, 'strm) BASIC_WRITER) :(char, 'strm) WRITER =
    fn c => basic_writer (char_cvt c)

  fun unit (basic_writer :(string, 'strm) BASIC_WRITER) :(unit, 'strm) WRITER =
    fn () => basic_writer "()"

  fun unit_ unit_cvt (basic_writer :(string, 'strm) BASIC_WRITER) :(unit, 'strm) WRITER =
    fn () => basic_writer (unit_cvt ())

  fun bool (basic_writer :(string, 'strm) BASIC_WRITER) :(bool, 'strm) WRITER =
    fn b => basic_writer (Bool.toString b)

  fun bool_ bool_cvt (basic_writer :(string, 'strm) BASIC_WRITER) :(bool, 'strm) WRITER =
    fn b => basic_writer (bool_cvt b)

  fun int (basic_writer :(string, 'strm) BASIC_WRITER) :(int, 'strm) WRITER =
    fn i => basic_writer (Int.toString i)

  fun int_ int_cvt (basic_writer :(string, 'strm) BASIC_WRITER) :(int, 'strm) WRITER =
    fn i => basic_writer (int_cvt i)        (* a user-defined conversion can avoid the strange SML ~ in negative numbers *)

  fun real (basic_writer :(string, 'strm) BASIC_WRITER) :(real, 'strm) WRITER =
    fn r => basic_writer (Real.toString r)

  fun real_ real_cvt (basic_writer :(string, 'strm) BASIC_WRITER) :(real, 'strm) WRITER =
    fn r => basic_writer (real_cvt r)

  fun string (basic_writer :(string, 'strm) BASIC_WRITER) :(string, 'strm) WRITER =
    fn s => fn ostr =>
       ( basic_writer "\"" ostr;
         basic_writer (String.toCString s) ostr;
         basic_writer "\"" ostr )

  fun string_ string_cvt  (basic_writer :(string, 'strm) BASIC_WRITER) :(string, 'strm) WRITER =
    fn s => basic_writer (string_cvt s)      (* e.g., one can use String.toString as string_cvt as an alternative *)

  fun list elem_writer (basic_writer :(string, 'strm) BASIC_WRITER) :('a list, 'strm) WRITER =
    fn (xs :'a list) => fn (ostr :'strm) =>
      let fun wrt x = (elem_writer basic_writer) x ostr
          fun iterate [] = ()
            | iterate [x] = wrt x
            | iterate (x::xs) = ( wrt x; basic_writer ", " ostr; iterate xs )
      in basic_writer "[" ostr;
         iterate xs;
         basic_writer "]" ostr
      end

  fun list_ (list_fmt_strg :string) (elem_fmt_strg :string) elem_writer (basic_writer :(string, 'strm) BASIC_WRITER) :('a list, 'strm) WRITER =
    fn (data_list :'a list) => fn (ostr :'strm) =>
       let fun write_str x  = basic_writer x ostr
           fun write_elem x = (elem_writer basic_writer) x ostr
           val list_fmt_list = parseFormatString list_fmt_strg
           val elem_fmt_list = parseFormatString elem_fmt_strg
           val use_separators = (List.find (fn "$2" => true | _ => false) elem_fmt_list) <> NONE
           fun F_inner_with_sep [] = ()
             | F_inner_with_sep [data_elem] =
                 let fun write_data_elem_without_sep [] = ()
                       | write_data_elem_without_sep (fmt_list as ("$1"  :: _))        = write_elem data_elem
                       | write_data_elem_without_sep (fmt_list as (fmt_x :: fmt_rest)) = write_data_elem_without_sep ( write_str fmt_x; fmt_rest )
                 in write_data_elem_without_sep elem_fmt_list
                 end
             | F_inner_with_sep (data_elem :: data_rest) =
                 let fun write_data_elem_with_sep [] = ()
                       | write_data_elem_with_sep (fmt_list as ("$1" :: fmt_rest))  = write_data_elem_with_sep ( write_elem data_elem; fmt_rest )
                       | write_data_elem_with_sep (fmt_list as ("$2" :: _))         = ()
                       | write_data_elem_with_sep (fmt_list as (fmt_x :: fmt_rest)) = write_data_elem_with_sep ( write_str fmt_x; fmt_rest )
                 in F_inner_with_sep ( write_data_elem_with_sep elem_fmt_list; data_rest )
                 end
           fun F_inner_simple data_list =
             app (fn data_elem => app (fn "$1" => write_elem data_elem | fmt_x  => write_str fmt_x) elem_fmt_list) data_list
       in let val F_inner = if use_separators then F_inner_with_sep else F_inner_simple
          in app (fn x => if x = "$$" then F_inner data_list else write_str x) list_fmt_list
          end
       end

  fun repl0args _     basic_writer ostr () fmt_x = basic_writer fmt_x ostr

  fun repl1arg writer basic_writer ostr x "$1"  = writer basic_writer x ostr
    | repl1arg _      basic_writer ostr _ fmt_x = basic_writer fmt_x ostr

  fun repl2args (writer1, writer2) basic_writer ostr (x1, x2) "$1"   = writer1 basic_writer x1 ostr
    | repl2args (writer1, writer2) basic_writer ostr (x1, x2) "$2"   = writer2 basic_writer x2 ostr
    | repl2args _                  basic_writer ostr _        fmt_x  = basic_writer fmt_x ostr

  fun repl3args (writer1, _, _) basic_writer ostr (x1, _, _) "$1"   = writer1 basic_writer x1 ostr
    | repl3args (_, writer2, _) basic_writer ostr (_, x2, _) "$2"   = writer2 basic_writer x2 ostr
    | repl3args (_, _, writer3) basic_writer ostr (_, _, x3) "$3"   = writer3 basic_writer x3 ostr
    | repl3args _                  basic_writer ostr _        fmt_x = basic_writer fmt_x ostr

  fun repl4args (writer1, _, _, _) basic_writer ostr (x1, _, _, _) "$1"   = writer1 basic_writer x1 ostr
    | repl4args (_, writer2, _, _) basic_writer ostr (_, x2, _, _) "$2"   = writer2 basic_writer x2 ostr
    | repl4args (_, _, writer3, _) basic_writer ostr (_, _, x3, _) "$3"   = writer3 basic_writer x3 ostr
    | repl4args (_, _, _, writer4) basic_writer ostr (_, _, _, x4) "$4"   = writer4 basic_writer x4 ostr
    | repl4args _                  basic_writer ostr _              fmt_x = basic_writer fmt_x ostr

  fun repl5args (writer1, _, _, _, _) basic_writer ostr (x1, _, _, _, _) "$1"   = writer1 basic_writer x1 ostr
    | repl5args (_, writer2, _, _, _) basic_writer ostr (_, x2, _, _, _) "$2"   = writer2 basic_writer x2 ostr
    | repl5args (_, _, writer3, _, _) basic_writer ostr (_, _, x3, _, _) "$3"   = writer3 basic_writer x3 ostr
    | repl5args (_, _, _, writer4, _) basic_writer ostr (_, _, _, x4, _) "$4"   = writer4 basic_writer x4 ostr
    | repl5args (_, _, _, _, writer5) basic_writer ostr (_, _, _, _, x5) "$5"   = writer5 basic_writer x5 ostr
    | repl5args _                     basic_writer ostr _              fmt_x = basic_writer fmt_x ostr

  fun replace0 basic_writer writer  x       fmt_strg ostr = app (repl0args writer  basic_writer ostr x)       (parseFormatString fmt_strg)
  fun replace1 basic_writer writer  x       fmt_strg ostr = app (repl1arg  writer  basic_writer ostr x)       (parseFormatString fmt_strg)
  fun replace2 basic_writer writers x_tuple fmt_strg ostr = app (repl2args writers basic_writer ostr x_tuple) (parseFormatString fmt_strg)
  fun replace3 basic_writer writers x_tuple fmt_strg ostr = app (repl3args writers basic_writer ostr x_tuple) (parseFormatString fmt_strg)
  fun replace4 basic_writer writers x_tuple fmt_strg ostr = app (repl4args writers basic_writer ostr x_tuple) (parseFormatString fmt_strg)
  fun replace5 basic_writer writers x_tuple fmt_strg ostr = app (repl5args writers basic_writer ostr x_tuple) (parseFormatString fmt_strg)

  infix 5 ++
  fun (writer1 ++ writer2) basic_writer (x1, x2) ostr =
    (writer1 basic_writer x1 ostr; writer2 basic_writer x2 ostr)
  
  fun empty basic_writer x =
    (const literal "") basic_writer x
                         
  fun single_ fmt_strg writer basic_writer x =
    replace1 basic_writer writer x fmt_strg

  fun single writer basic_writer x ostr =
  (* just the argument in parentheses *)
    single_ "($1)" writer basic_writer x ostr

  fun datatype1 proj1 basic_writer x ostr =
      ( let val (tag1, args1, writer1) = proj1 x
        in (literal ++ writer1) basic_writer (tag1 ^ " ", args1) ostr end )
      
  fun datatype1_ proj1 (basic_writer :(string, 'strm) BASIC_WRITER) (x :'a) (ostr :'strm) =
      ( let val (fmtStr1, args1, replaceNfct1, writers1) = proj1 x
        in replaceNfct1 basic_writer writers1 args1 fmtStr1 ostr end )

  fun datatype2 (proj1, proj2) basic_writer x ostr =
      ( let val (tag1, args1, writer1) = proj1 x
        in (literal ++ writer1) basic_writer (tag1 ^ " ", args1) ostr end )
      handle Match => (fn (tag2, args2, writer2) => (literal ++ writer2) basic_writer (tag2 ^ " ", args2) ostr) (proj2 x)
      
  fun datatype2_ ( proj1 :'a -> (string * 'a1 * (((string, 'strm) BASIC_WRITER) -> 'writers1 -> 'a1 -> string -> 'strm -> unit) * 'writers1),
                   proj2 :'a -> (string * 'a2 * (((string, 'strm) BASIC_WRITER) -> 'writers2 -> 'a2 -> string -> 'strm -> unit) * 'writers2) )
                 (basic_writer :(string, 'strm) BASIC_WRITER) (x :'a) (ostr :'strm) =
      ( let val (fmtStr1, args1, replaceNfct1, writers1) = proj1 x
        in replaceNfct1 basic_writer writers1 args1 fmtStr1 ostr end )
      handle Match => (fn (fmtStr2, args2, replaceNfct2, writers2) => replaceNfct2 basic_writer writers2 args2 fmtStr2 ostr) (proj2 x)

  (* EXAMPLES:
  datatype T2 = A of int | B of int * bool

  fun write_T2 basic_writer x ostr =
    datatype2 ( fn A i      => ("A", i, single int)            | _ => raise Match,
                fn B (i, b) => ("B", (i, b), pair (int, bool)) | _ => raise Match ) basic_writer x ostr
                                      
  fun write_T2 basic_writer x ostr =
    datatype2_ ( fn A i      => ("A ($1)", i, replace1, int)                  | _ => raise Match,
                 fn B (i, b) => ("B ($1, $2)", (i, b), replace2, (int, bool)) | _ => raise Match ) basic_writer x ostr
  *)

  fun option value_writer basic_writer :('a option, 'strm) WRITER =
    fn x => fn ostr =>
       let fun $ s = basic_writer s ostr
       in case x of
             SOME value => ( $"SOME "; single value_writer basic_writer value ostr )
           | NONE       => $"NONE"
       end

  (* alternative implementation using datatype2:
  fun option value_writer basic_writer x ostr =
    datatype2 ( fn SOME value => ("SOME", value, single value_writer) | _ => raise Match,
                fn NONE       => ("NONE", (), empty) | _ => raise Match ) basic_writer x ostr
  *) 

  fun option_ (fmt_strg_some, fmt_strg_none) value_writer basic_writer :('a option, 'strm) WRITER =
      fn SOME value => replace1 basic_writer value_writer value fmt_strg_some 
       | NONE       => replace0 basic_writer empty () fmt_strg_none   (* simply output the string, because there are no arguments anyway *) 

  fun pair_ fmt_strg (writer1, writer2) basic_writer (x1, x2) =
    replace2 basic_writer (writer1, writer2) (x1, x2) fmt_strg

  fun pair (writer1, writer2) basic_writer :('a1  * 'a2, 'strm) WRITER =
  (* this one is coded by hand, just as an example *)
    fn (x1, x2) => fn ostr =>
       let fun $ s = basic_writer s ostr
           fun % writer x = writer basic_writer x ostr
       in $"("; %writer1 x1; $", "; %writer2 x2; $")"
       end

  fun triple_ fmt_strg writers basic_writer (xs as (_, _, _)) =
    replace3 basic_writer writers xs fmt_strg

  fun triple writers basic_writer xs ostr =
    triple_ "($1, $2, $3)" writers basic_writer xs ostr
      
  fun tuple4_ fmt_strg writers basic_writer (xs as (_, _, _, _)) =
    replace4 basic_writer writers xs fmt_strg

  fun tuple4 writers basic_writer xs ostr =
    tuple4_ "($1, $2, $3, $4)" writers basic_writer xs ostr
      
  fun tuple5_ fmt_strg writers basic_writer (xs as (_, _, _, _, _)) =
    replace5 basic_writer writers xs fmt_strg
               
  fun tuple5 writers basic_writer xs ostr =
    tuple5_ "($1, $2, $3, $4, $5)" writers basic_writer xs ostr

  fun datatype3 (proj1, proj2, proj3) basic_writer x ostr =
    ( datatype2 (proj1, proj2) basic_writer x ostr )
    handle Match => (fn (tag3, args3, writer3) => (literal ++ writer3) basic_writer (tag3 ^ " ", args3) ostr) (proj3 x)

  fun datatype3_ (proj1, proj2, proj3) basic_writer x ostr =
    ( datatype2_ (proj1, proj2) basic_writer x ostr )
    handle Match => (fn (fmtStr3, args3, replaceNfct3, writers3) => replaceNfct3 basic_writer writers3 args3 fmtStr3 ostr) (proj3 x)

  fun datatype4 (proj1, proj2, proj3, proj4) basic_writer x ostr =
    ( datatype3 (proj1, proj2, proj3) basic_writer x ostr )
    handle Match => (fn (tag4, args4, writer4) => (literal ++ writer4) basic_writer (tag4 ^ " ", args4) ostr) (proj4 x)

  fun datatype4_ (proj1, proj2, proj3, proj4) basic_writer x ostr =
    ( datatype3_ (proj1, proj2, proj3) basic_writer x ostr )
    handle Match => (fn (fmtStr4, args4, replaceNfct4, writers4) => replaceNfct4 basic_writer writers4 args4 fmtStr4 ostr) (proj4 x)

  fun datatype5 (proj1, proj2, proj3, proj4, proj5) basic_writer x ostr =
    ( datatype4 (proj1, proj2, proj3, proj4) basic_writer x ostr )
    handle Match => (fn (tag5, args5, writer5) => (literal ++ writer5) basic_writer (tag5 ^ " ", args5) ostr) (proj5 x)

  fun datatype5_ (proj1, proj2, proj3, proj4, proj5) basic_writer x ostr =
    ( datatype4_ (proj1, proj2, proj3, proj4) basic_writer x ostr )
    handle Match => (fn (fmtStr5, args5, replaceNfct5, writers5) => replaceNfct5 basic_writer writers5 args5 fmtStr5 ostr) (proj5 x)
end

(* ************************************************************************* *)

signature BasicWriter_sig =
sig
  type STREAM
  type BASIC_WRITER

  val writer : BASIC_WRITER
end

(* ************************************************************************* *)

structure StringOutputStream =
struct
  type STREAM = string list ref
  fun newStream () :STREAM = ref []
  fun getString (ostr :STREAM) = String.concat (List.rev (!ostr))
end


structure StringBasicWriter : BasicWriter_sig =
struct
  type STREAM = StringOutputStream.STREAM
  type BASIC_WRITER = (string, STREAM) Write.BASIC_WRITER
  fun writer (s :string) (ostringlist :STREAM) :unit = ostringlist := s :: (!ostringlist)
end

(* ************************************************************************* *)
  
structure FileBasicWriter : BasicWriter_sig =
struct
  type STREAM = TextIO.outstream
  type BASIC_WRITER = (string, STREAM) Write.BASIC_WRITER
                
  fun writer (s :string) (ostream :STREAM) :unit = TextIO.output (ostream, s)
end

(* ************************************************************************* *)
  
signature Output_sig =
sig 
  val toString   : (StringBasicWriter.BASIC_WRITER -> ('a, StringBasicWriter.STREAM) Write.WRITER) ->
                   'a -> string

  val toOpenFile : TextIO.outstream ->
                   (FileBasicWriter.BASIC_WRITER -> ('a, FileBasicWriter.STREAM) Write.WRITER) ->
                   'a -> unit

  val toFile     : string ->    (* filename *)
                   (FileBasicWriter.BASIC_WRITER -> ('a, FileBasicWriter.STREAM) Write.WRITER) ->
                   'a -> unit
end

(* ************************************************************************* *)
  
structure Output : Output_sig =
struct
  fun toString writer data =
    let val ostr = StringOutputStream.newStream ()
    in writer StringBasicWriter.writer data ostr;
       StringOutputStream.getString ostr
    end

  fun toOpenFile file writer data =
    writer FileBasicWriter.writer data file

  fun toFile filename writer data =
    let val f = TextIO.openOut filename
    in writer FileBasicWriter.writer data f; 
       TextIO.closeOut f
    end
end

(* ************************************************************************* *)
