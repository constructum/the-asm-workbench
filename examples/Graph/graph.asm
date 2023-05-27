typealias NODE == INT
typealias EDGE == INT * INT

typealias GRAPH == SET(NODE) * SET(EDGE)


static function G :GRAPH ==
  ( { 1..7 },
    { (1,2), (1,3), (2,3), (4,3), (3,5), (5,2), (5,6), (6,7), (7,4) } )

static function nodes ((N,E) :GRAPH) == N
static function edges ((N,E) :GRAPH) == E


static function initialNode  :NODE == 1
static function finalNode    :NODE == 4

dynamic function currentNode :NODE
with currentNode in nodes (G)
initially initialNode

external function chooseNode :NODE
with chooseNode in nodes (G)


rule SearchPath ==
  if currentNode != finalNode
  then if member (chooseNode, { y | (x,y) in edges (G) with x = currentNode })
       then currentNode := chooseNode
       endif
  endif
