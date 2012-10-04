signature ExportHelper =
sig
	val get_consts : theory -> (string * (typ * term option)) list
	val get_gen_consts : theory -> string -> (string * Datatype.info) list -> string list
	val get_axioms : theory -> (string * term) list
	val get_gen_axioms : theory -> string -> (string * Datatype.info) list -> string list
	val get_theorems : theory -> (string * term) list
	val get_gen_theorems : theory -> string -> (string * Datatype.info) list -> string list -> string list
	val get_datatypes : theory -> (string * Datatype.info) list
	val filter : string list -> (string * 'a) list -> (string * 'a) list
	val termListToXML : theory -> string -> (string * term) list -> XML.tree
	val termTypListToXML : theory -> string -> (string * (typ * term option)) list -> XML.tree
	val typesListToXML : string -> (string * Datatype.info) list -> XML.tree
        val fixTypeNames : string -> XML.tree -> XML.tree
end;

structure ExportHelper : ExportHelper =
struct
	fun id x = x
	fun mergesort _ []  	   = []
	  | mergesort _ [x] 	   = [x]
	  | mergesort cmp_data xs  = let val (ys,zs) = List.foldl (fn(i, (ys,zs)) => (zs, i::ys)) ([],[]) xs
			          	 fun merge (xs,[]) = xs
				    	   | merge ([],xs) = xs
				    	   | merge (x::xr,y::yr) = if (cmp_data x) <= (cmp_data y)
									then x::merge(xr,y::yr)
								   else y::merge(x::xr,yr)
			      	     in merge(mergesort cmp_data ys, mergesort cmp_data zs) end
	(* make a sorted list unique*)
	fun unique [] 		= []
          | unique [x]		= [x]
          | unique (x1::x2::xr) = if x1 = x2
                                   then unique (x2::xr)
                                  else x1::(unique (x2::xr))
	(* remove xs from ys - both lists need to be sorted asc *)
	fun remove' _ _ ([],ys)           = ys
	  | remove' _ _ (_,[])		  = []
	  | remove' cd1 cd2 (x::xr,y::yr) = if (cd1 x) < (cd2 y)
		 				then remove' cd1 cd2 (xr,y::yr)
		  		     	    else if (cd2 y) < (cd1 x)
						then y::(remove' cd1 cd2 (x::xr,yr))
				     	    else remove' cd1 cd2 (xr,yr)
	fun remove cmp_data = remove' cmp_data cmp_data
	fun remove_parent_data df cmp_data T = let val d  = df T
					           val pd = (List.foldl op@ [] (map df (Context.parents_of T)))
                                      	       in remove cmp_data (mergesort cmp_data pd,mergesort cmp_data d) end
	fun full_name T name = Name_Space.full_name (Sign.naming_of T) (Binding.name name)
	fun prefix p s = List.map (fn x => p^s^x)
        fun postfix s p = List.map (fn x => x^s^p)
	fun l_to_intl l = List.map Int.toString (List.tl (List.rev (List.foldl (fn (_,x::xs) => (x+1)::x::xs) [0] l)))
	fun is_mutually_rec_type T (n,i) = (case (#alt_names i) of
		SOME(names) => List.exists
  				(Option.isSome o (Datatype.get_info T) o (full_name T))
                                names
               | NONE => false)
	fun check_rec T (n,v) = if is_mutually_rec_type T (n,v)
         then case String.compare (((#1 o #2 o List.hd o #descr) v),full_name T n) of
               EQUAL => true
               | _ => false
         else true
        fun restructure_rec_types T ts = List.foldl (fn (d,l) => case d of
         (n,SOME(v)) => if check_rec T (n,v) then (n,v)::l
                        else l
         | (_,NONE) => l) [] ts
	fun get_type_names T ts =
            let
	     val rec_types = List.filter (is_mutually_rec_type T) ts
	     val grouped_rec_names = List.map (fn x => case (#alt_names o #2) x of
                               SOME(v) => v
                             | NONE => []) rec_types
  	     val def_names = (List.map (Long_Name.base_name o #1 o #2) (List.concat (List.map (#descr o #2) rec_types)))
	     val all_rec_names = (List.concat grouped_rec_names)@def_names
	     val constructors = List.map #1 (List.concat (List.map (#3 o #2) (List.concat (List.map (#descr o #2) ts))))
            in (grouped_rec_names,all_rec_names,constructors,def_names) end
	fun get_consts T = let val consts_of = (Name_Space.dest_table (Syntax.init_pretty_global T)) o #constants o Consts.dest o Sign.consts_of
			   in remove_parent_data consts_of #1 T end
	fun get_gen_consts T name ts = let val (grouped_rec_names,all_rec_names,constructors,_) = get_type_names T ts
					   val rec_names = List.concat grouped_rec_names
					   val mutually_rec_names = List.filter (fn x => List.length x > 1) grouped_rec_names
					   val simple_names = List.map List.hd (List.filter (fn x => List.length x = 1) grouped_rec_names)
                                     in constructors@(prefix name "." (List.concat
[prefix "Abs" "_" rec_names,
 prefix "Rep" "_" rec_names,
 List.map (fn x => "equal_"^x^"_inst.equal_"^x) all_rec_names,
 List.concat (List.map (fn x => let val comb = space_implode "_" x
  in (List.map (fn y => comb^"."^y^"_case") x) end) grouped_rec_names),
 List.concat (List.map (fn x => [x^"."^x^"_rec",
				 x^"."^x^"_rec_set",
				 x^"."^x^"_rep_set"]) simple_names),
 List.concat (List.map
  (fn x => let val comb = space_implode "_" x
    in (comb^"."^comb^"_rec_set")::
       (comb^"."^comb^"_rep_set")::(List.concat
     (List.map 
      (fn y => [comb^"."^comb^"_rec_"^y,
                comb^"."^comb^"_rec_set_"^y,
                comb^"."^comb^"_rep_set_"^y]) (l_to_intl x))) end) mutually_rec_names)])) end
	val get_axioms = remove_parent_data Theory.axioms_of #1
	fun get_gen_axioms T name ts = let val (grouped_rec_names,all_rec_names,constructors,def_names) = get_type_names T ts
                                           val rec_names = List.concat grouped_rec_names
                                           val mutually_rec_names = List.filter (fn x => List.length x > 1) grouped_rec_names
                                           val simple_names = List.map List.hd (List.filter (fn x => List.length x = 1) grouped_rec_names)
        in (postfix "_" "def" constructors)@(prefix name "." (List.concat 
         [prefix "arity_type" "_" def_names,
          prefix "equal" "_" (postfix "_" "def_raw" def_names),
	  prefix "type_definition" "_" rec_names,
	  List.map (fn x => "equal_"^x^"_inst.equal_"^x^"_def") def_names,
	  (postfix "_" "def"
           (List.concat [(List.concat (List.map
  (fn x => let val comb = space_implode "_" x
    in (comb^"."^comb^"_rec_set")::
       (comb^"."^comb^"_rep_set")::(List.concat
     (List.map
      (fn y => [comb^"."^comb^"_rec_"^y,
                comb^"."^comb^"_rec_set_"^y,
                comb^"."^comb^"_rep_set_"^y]) (l_to_intl x))) end) mutually_rec_names)),
            List.concat (List.map (fn x => let val comb = space_implode "_" x
  in (List.map (fn y => comb^"."^y^"_case") x) end) grouped_rec_names),
            List.concat (List.map (fn x => [x^"."^x^"_rec",
                                 x^"."^x^"_rec_set",
                                 x^"."^x^"_rep_set"]) simple_names)]))])) end
	val get_theorems = let fun theorems_of T =  map (fn (a,b) => (a, prop_of b)) (Global_Theory.all_thms_of T)
			   in remove_parent_data theorems_of #1 end
	fun get_gen_theorems T name ts ths = let val (grouped_rec_names,all_rec_names,constructors,def_names) = get_type_names T ts
                                           val rec_names = List.concat grouped_rec_names
                                           val mutually_rec_names = List.filter (fn x => List.length x > 1) grouped_rec_names
                                           val simple_names = List.map List.hd (List.filter (fn x => List.length x = 1) grouped_rec_names)
        in prefix name "." (List.concat
         [prefix "arity_equal" "_" def_names,
          prefix "arity_type" "_" def_names,
	  prefix "type_definition" "_" rec_names,
	  prefix "equal" "_" (postfix "_" "def" def_names),
          prefix "equal" "_" (postfix "_" "def_raw" def_names),
	  List.map (fn x => "equal_"^x^"_inst.equal_"^x) def_names])
         @(List.concat (List.map (fn x =>
            List.filter (String.isPrefix (name^"."^x^".")) ths) (unique (rec_names@def_names))))
         @(List.concat (List.map (fn x =>
            List.filter (String.isPrefix (name^"."^(space_implode "_" x)^".")) ths) mutually_rec_names)) end
	fun get_datatypes T = let val tname = Context.theory_name T
			          val tl = String.size tname
                                  val ts = (#log_types o Type.rep_tsig o Sign.tsig_of) T
                                  val ts' = map (fn s => (String.extract (s,tl+1,NONE),Datatype.get_info T s)) (List.filter (String.isPrefix tname) ts)
			      in restructure_rec_types T ts' end
	fun filter rem d = remove' id #1 ((mergesort id rem),(mergesort #1 d))
        fun remove_hol_true_prop t = case t of 
           $ (Const ("HOL.Trueprop",_), tm) => tm
         | (t $ u) => (remove_hol_true_prop t) $ (remove_hol_true_prop u)
         | Abs (s,T,t) => Abs (s,T,remove_hol_true_prop t)
         | tm => tm
        fun xml_of_term' T t =
          case t of
           XML.Elem (("Const",a),t) =>
              let val b =
                      (case (Syntax.guess_infix (Sign.syn_of T) (Lexicon.mark_const ((#2 o List.hd) a))) of
                          SOME(Mixfix.Infixl (s,j)) =>
                                               [("infixl",s),
                                                ("mixfix_i",string_of_int j)]
                        | SOME(Mixfix.Infixr (s,j)) =>
                                               [("infixr",s),
                                                ("mixfix_i",string_of_int j)]
                        | SOME(Mixfix.Infix (s,j))  =>
                                               [("infix",s),
                                                ("mixifix_i",string_of_int j)]
                        | NONE => [])
              in XML.Elem (("Const",a@b),map (xml_of_term' T) t) end
           | XML.Elem ((s,a),t) => XML.Elem ((s,a),map (xml_of_term' T) t)
        fun xml_of_term T t = xml_of_term' T (XML_Syntax.xml_of_term t)
	fun termListToXML T section l = XML.Elem ((section,[]),List.map (
         fn (s,t) => XML.Elem (("Term",[("name",Long_Name.base_name s)]),[xml_of_term T (remove_hol_true_prop t)])) l)
	fun termTypListToXML T section l = XML.Elem ((section,[]),List.map (
	 fn (s,(t,v)) => let val v' = case v of
          SOME(tm) => (("Term",[("name",Long_Name.base_name s)]),[xml_of_term T tm])
	  | NONE => (("NoTerm",[]),[])
         in XML.Elem (("ConstDecl",[("name",Long_Name.base_name s)]),[XML_Syntax.xml_of_type t,XML.Elem v']) end) l)
	fun dtypToXML (Datatype.DtTFree s) = XML.Elem (("DtTFree",[("s",s)]),[])
           | dtypToXML (Datatype.DtType (s,dtl)) = XML.Elem (("DtType",[("s",s)]),List.map dtypToXML dtl)
    	   | dtypToXML (Datatype.DtRec i) = XML.Elem (("DtRec",[("i",Int.toString i)]),[])
	fun constructorToXML (name,dtl) = XML.Elem
         (("Constructor",[("val",Long_Name.base_name name)]),List.map dtypToXML dtl)
	fun typeToXML d = List.map
         (fn (i,(s,vs,dt)) => let val attrs = case (#alt_names d) of
            SOME(ns) => if (List.nth (ns,i)) = (Long_Name.base_name s)
             then [("i",Int.toString i),("name",Long_Name.base_name s)]
             else [("i",Int.toString i),("name",Long_Name.base_name s),("altname",List.nth (ns,i))]
            | NONE => [("i",Int.toString i),("name",Long_Name.base_name s)]
          in XML.Elem (("RecType",attrs),[
               XML.Elem (("Vars",[]),List.map dtypToXML vs),
	       XML.Elem (("Constructors",[]),List.map constructorToXML dt)
              ])
          end)
         (#descr d)
	fun typesListToXML section l = XML.Elem ((section,[]),List.map (
	fn (s,d) => XML.Elem (("TypeDecl",[("name",s)]),typeToXML d)) l)
        fun fixTypeNames moduleName t = case t of
             XML.Elem (("Type",a),c) => XML.Elem (("Type",
                List.map (fn (n,s) =>
                 if n = "name" andalso String.isPrefix (moduleName^".") s
                  then (n,String.extract (s,(String.size moduleName)+1,NONE))
                  else (n,s)) a),
                List.map (fixTypeNames moduleName) c)
             | XML.Elem (d,c) =>
                XML.Elem (d,List.map (fixTypeNames moduleName) c)
             | _ => t
end;
