type BST =
    | Empty
    | TreeNode of int * BST * BST

let rec insert value tree =
    match tree with
    | Empty -> TreeNode (value, Empty, Empty)
    | TreeNode(vl, l, r) ->
    if value > vl then
        TreeNode (vl, l, insert value r)
    else
        TreeNode (vl, insert value l, r)


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


let rec count func tree =
    match tree with 
    | Empty -> 0
    |TreeNode(vl,l,r ) ->
        let result = (count func l) + (count  func r)
        if func vl then result + 1
        else 
            result

let evenount tree =
    count(fun x -> x % 2 = 0)tree