// Jaidan Dovala
// 30 April 2022
// C3300
// pa4.fs
// This assignment consists of three F# exercises

open System

// function that takes a list of floating-point tuples that represent dimensions of 
// a cube (length, width, and height) and returns the volume of the cube that has the largest volume
let rec maxCubeVolume = function
    | [] -> 0.0
    | (l, w, h) :: tl -> 
        let max = maxCubeVolume tl
        let maxVolume = l * w * h
        if maxVolume > max then maxVolume
        else maxVolume

// ------------------------------------------------------------------

// function findMatches that takes a string and a list of tuples as arguments. 
// Each element of the list will be a tuple consisting of a string and an int. 
// Finding all of the tuples for which the string matches the first argument and collect all of the corresponding integers.
// final result are collected integers sorted in ascending order.
let rec findMatches find = function
    | [] -> []: int list 
    | (a,b) :: tl ->
        if a = find then
            List.sort(b::findMatches find tl)
        else
            List.sort (findMatches find tl)

        
// ------------------------------------------------------------------

// Tree definition for problem 3
type BST =
    | Empty
    | TreeNode of int * BST * BST

//Inserts the value into the tree and returns the resulting tree.
//int -> tree: BST -> BST
let rec insert value tree = 
    match tree with
    | Empty -> TreeNode (value, Empty, Empty)
    | TreeNode(vl, l, r) ->
    if value > vl then
        TreeNode (vl, l, insert value r)
    else
        TreeNode (vl, insert value l, r)

//Returns true if the value is in the tree or false if it is not.
//int -> tree: BST -> bool
let rec contains value tree = 
    match tree with
    | Empty -> false
    | TreeNode(vl, l, r) ->
    if value > vl then
        contains value r
    elif value < vl then
        contains value l
    else
        true

//The parameter func is a Boolean function that takes a single parameter and returns true or false.
//The function tests the value of each node with func and returns the number of nodes that evaluate to true.
//(int -> bool) -> tree: BST -> int
let rec count func tree = 
    match tree with 
    | Empty -> 0
    |TreeNode(vl,l,r ) ->
        if func vl = true then 
            1 + count func l + count func r
		else 
            count func l + count func r

//Returns the number of nodes that contain even integers.
//BST -> int
let evenCount tree = 
    count(fun x -> x % 2 = 0)tree