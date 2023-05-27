signature NodeSet_sig =
sig
  type NODE and NODE_SET

  val empty     :NODE_SET
  val singleton :NODE -> NODE_SET
  val numItems  :NODE_SET -> int
  val listItems :NODE_SET -> NODE list
  val toList    :NODE_SET -> NODE list   (* synonymous of listItems *)
  val member    :NODE_SET * NODE -> bool

  val union        :NODE_SET * NODE_SET -> NODE_SET
  val intersection :NODE_SET * NODE_SET -> NODE_SET
  val difference   :NODE_SET * NODE_SET -> NODE_SET
end


signature EdgeSet_sig =
sig
  type EDGE and EDGE_SET

  val empty     :EDGE_SET
  val singleton :EDGE -> EDGE_SET
  val numItems  :EDGE_SET -> int
  val listItems :EDGE_SET -> EDGE list
  val toList    :EDGE_SET -> EDGE list   (* synonymous of listItems *)
  val member    :EDGE_SET * EDGE -> bool

  val union        :EDGE_SET * EDGE_SET -> EDGE_SET
  val intersection :EDGE_SET * EDGE_SET -> EDGE_SET
  val difference   :EDGE_SET * EDGE_SET -> EDGE_SET
end


signature Graph_sig =
sig
  type NODE_LABEL and NODE and NODE_SET
  type EDGE_LABEL and EDGE and EDGE_SET
  type GRAPH

  exception InvariantViolated of string
  exception GraphOperation of string

  structure NodeSet :NodeSet_sig
  sharing type NodeSet.NODE     = NODE
  sharing type NodeSet.NODE_SET = NODE_SET

  structure EdgeSet :EdgeSet_sig
  sharing type EdgeSet.EDGE     = EDGE
  sharing type EdgeSet.EDGE_SET = EDGE_SET

  val mkGraph         :unit -> GRAPH
  val mkDirectedGraph :unit -> GRAPH

  val replicate :GRAPH -> GRAPH
  val copy	:GRAPH -> GRAPH     (* synonymous of 'replicate' *)

  val numNodes  :GRAPH -> int
  val numEdges  :GRAPH -> int

  val nodeSet   :GRAPH -> NODE_SET
  val edgeSet   :GRAPH -> EDGE_SET

  val listNodes :GRAPH -> NODE list
  val listEdges :GRAPH -> EDGE list

  val addNode   :GRAPH -> NODE_LABEL -> NODE
  val addEdge	:GRAPH -> NODE * EDGE_LABEL * NODE -> EDGE

  val removeNode :GRAPH -> NODE -> unit
  val removeEdge :GRAPH -> EDGE -> unit
  val removeAll  :GRAPH -> unit

  val connectNode :GRAPH -> (NODE * EDGE_LABEL) list -> NODE -> (EDGE_LABEL * NODE) list -> unit

  val nodeLabel :NODE -> NODE_LABEL
  val edgeLabel :EDGE -> EDGE_LABEL

  val updateNodeLabel :NODE * NODE_LABEL -> unit
  val updateEdgeLabel :EDGE * EDGE_LABEL -> unit

  val inEdges       :NODE -> EDGE_SET
  val outEdges      :NODE -> EDGE_SET
  val incidentEdges :NODE -> EDGE_SET     (* for undirected graphs *)

  val preNodes      :NODE -> NODE_SET
  val preNodesStar  :NODE -> NODE_SET
  val preNodesPlus  :NODE -> NODE_SET

  val postNodes     :NODE -> NODE_SET
  val postNodesStar :NODE -> NODE_SET
  val postNodesPlus :NODE -> NODE_SET

  val adjacentNodes :NODE -> NODE_SET     (* for undirected graphs *)

  val from :EDGE -> NODE
  val to   :EDGE -> NODE
  val ends :EDGE -> NODE * NODE           (* for undirected graphs *)

  val selectNodes :GRAPH -> (NODE_LABEL -> bool) -> NODE_SET
  val selectEdges :GRAPH -> (EDGE_LABEL -> bool) -> EDGE_SET
end


functor Graph (Labels :sig type NODE_LABEL type EDGE_LABEL end) :Graph_sig =
struct
  type NODE_LABEL = Labels.NODE_LABEL
  type EDGE_LABEL = Labels.EDGE_LABEL

  datatype NODE =
    Node of {
      index    : int,
      label    : NODE_LABEL ref,
      in_list  : EDGE list ref,
      out_list : EDGE list ref
    } ref
  and EDGE =
    Edge of {
      index : int,
      label : EDGE_LABEL ref,
      from  : NODE,
      to    : NODE
    } ref

  structure NodeSet =
    struct
      type NODE = NODE
      structure Set = BinarySetFn ( struct
	type ord_key = NODE
        fun compare (Node (ref n1), Node (ref n2)) = Int.compare (#index n1, #index n2)
      end )
      type NODE_SET = Set.set
      open Set
      val toList = listItems
    end

  structure EdgeSet =
    struct
      type EDGE = EDGE
      structure Set = BinarySetFn ( struct
	type ord_key = EDGE
	fun compare (Edge (ref e1), Edge (ref e2)) = Int.compare (#index e1, #index e2)
      end )
      type EDGE_SET = Set.set
      open Set
      val toList = listItems
    end

  type NODE_SET = NodeSet.NODE_SET
  type EDGE_SET = EdgeSet.EDGE_SET

  datatype GRAPH =
    Graph of {
      directed : bool,
      lastNodeIndex : int ref,
      lastEdgeIndex : int ref,
      nodes    : NODE_SET ref,
      edges    : EDGE_SET ref
    } ref

  exception InvariantViolated of string
  exception GraphOperation of string

  local
    fun mkAnyGraph isDirected =
      Graph (ref {
        directed = isDirected,
	lastNodeIndex = ref 0,
	lastEdgeIndex = ref 0,
	nodes = ref (NodeSet.empty),
	edges = ref (EdgeSet.empty)
      })
  in fun mkGraph () = mkAnyGraph false
     fun mkDirectedGraph () = mkAnyGraph true
  end

  fun newNodeIndex (Graph (ref G')) =
    let val new_index = (!(#lastNodeIndex G')) + 1
    in #lastNodeIndex G' := new_index;
       new_index
    end
  fun newEdgeIndex (Graph (ref G')) =
    let val new_index = (!(#lastEdgeIndex G')) + 1
    in #lastEdgeIndex G' := new_index;
       new_index
    end

  fun numNodes (Graph (ref G')) = NodeSet.numItems (!(#nodes G'))
  fun numEdges (Graph (ref G')) = EdgeSet.numItems (!(#edges G'))

  fun nodeSet (Graph (ref G')) = (!(#nodes G'))
  fun edgeSet (Graph (ref G')) = (!(#edges G'))

  fun listNodes (Graph (ref G')) = NodeSet.listItems (!(#nodes G'))
  fun listEdges (Graph (ref G')) = EdgeSet.listItems (!(#edges G'))

  fun addNode (G as Graph (ref G')) (label :NODE_LABEL) =
    let fun create_node (in_list, out_list) =
	  let val next_index = newNodeIndex G
	  in Node (ref { index = next_index, label = ref label, in_list = in_list, out_list = out_list })
	  end
        val new_node =
          if #directed G'
	  then create_node (ref [], ref [])
	  else (fn adj_list => create_node (adj_list, adj_list)) (ref [])
    in (#nodes G') := NodeSet.add (!(#nodes G'), new_node);
       new_node
    end

  fun addEdge (G as Graph (ref G')) (from as Node (ref from'), label :EDGE_LABEL, to as Node (ref to')) =
    let fun create_edge (from, to) =
          let val next_index = newEdgeIndex G
	  in Edge (ref { index = next_index, label = ref label, from = from, to = to })
	  end
        fun exists_edge (from as Node (ref from'), to) =
          let fun find [] = false
	        | find (Edge (ref { from = from0, to = to0, ... }) :: xs) =
		    ((from0 = from) andalso (to0 = to)) orelse (find xs)
          in find (!(#out_list from'))
	  end
        val new_edge =
	  if #directed G'
	  then if exists_edge (from, to)
	       then raise GraphOperation "addEdge"
	       else create_edge (from, to)
	  else if exists_edge (if #index from' <= #index to' then (from, to) else (to, from))
	       then raise GraphOperation "addEdge"
	       else create_edge (if #index from' <= #index to' then (from, to) else (to, from))
    in (#edges G')       := EdgeSet.add (!(#edges G'), new_edge);
       (#out_list from') := new_edge :: (!(#out_list from'));
       (#in_list to')    := new_edge :: (!(#in_list to'));
       new_edge
    end


  local
    structure Table = BinaryMapFn ( struct
      type ord_key = int
      val compare  = Int.compare
    end )
  in
    fun replicate (G as Graph (ref G')) =
      let val new_G = if #directed G' then mkDirectedGraph () else mkGraph ()
	  val node_table = ref (Table.empty :NODE Table.map)
	  fun replicate_node (n as (Node (ref n'))) =
	    let val new_n = (addNode new_G) (!(#label n'))
	    in node_table := Table.insert (!node_table, #index n', new_n)
	    end
	  fun replicate_edge (Edge (ref { from = Node (ref from'), to = Node (ref to'), label = ref label', ... })) =
	    (addEdge new_G) ( valOf (Table.find (!node_table, #index from')),
			      label',
                              valOf (Table.find (!node_table, #index to')) )
      in map replicate_node (listNodes G);
	 map replicate_edge (listEdges G);
	 new_G
      end
    val copy = replicate
  end

  fun removeEdge (Graph (ref G')) (e as Edge (ref e')) =
    let val (Node (ref from'), Node (ref to')) = (#from e', #to e')
    in (#edges G')       := EdgeSet.delete (!(#edges G'), e);
       (#out_list from') := List.filter (fn e_ => e_ <> e) (!(#out_list from'));
       (#in_list to')    := List.filter (fn e_ => e_ <> e) (!(#in_list to'))
    end

  fun removeNode (G as Graph (ref G')) (n as Node (ref n')) =
  ( map (removeEdge G) (!(#in_list n'));
    if #directed G'
    then map (removeEdge G) (!(#out_list n'))
    else if (#in_list n' <> #out_list n')
    then raise InvariantViolated "[removeNode]: undirected graph, in_list <> out_list"
    else [];
    (#nodes G') := NodeSet.delete (!(#nodes G'), n) )

  fun removeAll (G as Graph (ref G')) =
  ( map (removeNode G) (NodeSet.listItems (!(#nodes G')));
    (* ??? what about lastNodeIndex, lastEdgeIndex ??? *)
    () )

  fun connectNode (G as Graph (ref G'))
        (in_ :(NODE * EDGE_LABEL) list) (node :NODE) (out_ :(EDGE_LABEL * NODE) list) =
    ( map (fn (from, label) => addEdge G (from, label, node)) in_;
      map (fn (label, to) => addEdge G (node, label, to)) out_;
      () )

  fun nodeLabel (Node (ref { label = ref label, ... })) = label
  fun edgeLabel (Edge (ref { label = ref label, ... })) = label

  fun updateNodeLabel (Node (ref { label = label_ref, ... }), new_label) =
    label_ref := new_label
  fun updateEdgeLabel (Edge (ref { label = label_ref, ... }), new_label) =
    label_ref := new_label

  fun inEdges (Node (ref { in_list = ref in_list, ... })) =
    List.foldll EdgeSet.add EdgeSet.empty in_list
  fun outEdges (Node (ref { out_list = ref out_list, ... })) =
    List.foldll EdgeSet.add EdgeSet.empty out_list

  fun incidentEdges (n as Node (ref { out_list = out_list, in_list = in_list, ... })) =
    if in_list = out_list
    then inEdges n
    else raise InvariantViolated "[adjacentEdges]: undirected graph, in_list <> out_list"

  fun from (Edge (ref { from = from, ... })) = from
  fun to (Edge (ref { to = to, ... })) = to

  fun ends (Edge (ref { from = from as Node (ref from'), to = to as Node (ref to'), ... })) =
    if #index from' <= #index to'
    then (from, to)
    else raise InvariantViolated "[ends]: undirected graph, #index from > #index to"

  fun preNodes (Node (ref { in_list = ref in_list, ... })) =
    List.foldll (fn (S, e) => NodeSet.add (S, from e)) NodeSet.empty in_list
  fun postNodes (Node (ref { out_list = ref out_list, ... })) =
    List.foldll (fn (S, e) => NodeSet.add (S, to e)) NodeSet.empty out_list

  local
    fun closure (empty, card, combine, listItems) F x =
      let val Combine = (List.foldll combine empty)
	  val x' = combine (x, Combine (map F (listItems x)))
      in if card x' = card x
	 then x'
	 else closure (empty, card, combine, listItems) F x'
      end

    local open NodeSet
    in val nodeset_closure = closure (empty, numItems, union, listItems)
    end
  in
    fun postNodesStar n = nodeset_closure postNodes (NodeSet.singleton n)
    fun preNodesStar n  = nodeset_closure preNodes (NodeSet.singleton n)

    fun postNodesPlus n = nodeset_closure postNodes (postNodes n)
    fun preNodesPlus n  = nodeset_closure preNodes (preNodes n)
  end

  fun adjacentNodes (n as Node (ref { out_list = out_list, in_list = in_list, ... })) =
    let fun adjNode edge = (fn (f, t) => if f <> n then f else t) (from edge, to edge)
    in if in_list = out_list
       then NodeSet.difference (
	      List.foldll (fn (S, e) => NodeSet.add (S, adjNode e)) NodeSet.empty (!in_list),
	      NodeSet.singleton n )
       else raise InvariantViolated "[adjacentEdges]: undirected graph, in_list <> out_list"
    end

  fun selectNodes (Graph (ref G')) (property :NODE_LABEL -> bool) = 
    NodeSet.foldl
      (fn (n as Node (ref n'), S) => if property (!(#label n')) then NodeSet.add (S, n) else S)
      (!(#nodes G'))
      (NodeSet.empty)

  fun selectEdges (Graph (ref G')) (property :EDGE_LABEL -> bool) = 
    EdgeSet.foldl
      (fn (e as Edge (ref e'), S) => if property (!(#label e')) then EdgeSet.add (S, e) else S)
      (!(#edges G'))
      (EdgeSet.empty)
end
