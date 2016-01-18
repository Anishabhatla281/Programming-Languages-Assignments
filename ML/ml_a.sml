Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* Answer 1 *)
fun partition x [] = ([],[]) 
| partition x (y::ys) = let val (a,b) = partition x ys
in if (y<x) then (y::a, b) else (a, y::b)
end

(* Answer 2 *)
fun partitionSort [] = []
| partitionSort (x::xs) = let val (a,b) = partition x (partitionSort xs)
in a @ [x] @ b
end

(* Answer 3 *)
fun Sort (op <) [] = []
| Sort (op <) (x::xs) = let fun helper x [] = ([],[]) | helper x (y::ys) = let val (a,b) = helper x ys in if (y<x) then (y::a, b) else (a, y::b) end
val (c,d) = helper x (Sort (op <) xs)
in c @ [x] @ d
end

(* Answer 4 *)
datatype 'a tree = empty | leaf of 'a | node of 'a * 'a tree * 'a tree

(* Answer 5 *)
exception myException
fun maxTree (op <) empty = raise myException
| maxTree (op <) (leaf x) = x
| maxTree (op <) (node (label,lf,rt)) = let fun compare x y = if (x<y) then y else x
fun isEmpty(x) = case x of empty => true | _ => false
in if ((isEmpty(rt)) andalso not (isEmpty(lf))) then (compare (maxTree (op <) lf) label)
else if ((isEmpty(lf)) andalso not (isEmpty(rt))) then (compare (maxTree (op <) rt) label)
else if ((isEmpty(lf)) andalso (isEmpty(rt))) then label 
else compare (compare (maxTree (op <) lf) (maxTree (op <) rt)) label
end

(* Answer 6 *)
exception myException
fun Labels f empty = raise myException
| Labels f (leaf x) = [x]
| Labels f (node(label,lf,rt)) = let fun isEmpty(x) = case x of empty => true | _ => false
in if ((isEmpty(rt)) andalso not (isEmpty(lf))) then f 1 (node(label,lf,empty))
else if ((isEmpty(lf)) andalso not (isEmpty(rt))) then f 2 (node(label,empty,rt))
else if ((isEmpty(lf)) andalso (isEmpty(rt))) then [label] 
else f 3 (node(label,lf,rt))
end

fun preorder y empty = raise myException
| preorder y (leaf x) = [x]
| preorder y (node(label,lf,rt)) = case y of 
  1 => [label]@(Labels preorder lf)
| 2 => [label]@(Labels preorder rt)
| 3 => [label]@(Labels preorder lf)@(Labels preorder rt)
| _ => (Labels preorder empty)

fun inorder y empty = raise myException
| inorder y (leaf x) = [x]
| inorder y (node(label,lf,rt)) = case y of 
  1 => (Labels inorder lf)@[label]
| 2 => [label]@(Labels inorder rt)
| 3 => (Labels inorder lf)@[label]@(Labels inorder rt)
| _ => (Labels inorder empty)

fun postorder y empty = raise myException
| postorder y (leaf x) = [x]
| postorder y (node(label,lf,rt)) = case y of 
  1 => (Labels postorder lf)@[label]
| 2 => (Labels postorder rt)@[label]
| 3 => (Labels postorder lf)@(Labels postorder rt)@[label]
| _ => (Labels postorder empty)

(* Answer 7 *)
fun lexLess (op <) [] [] = false
| lexLess (op <) (x::xs) [] = false
| lexLess (op <) [] (x::xs) = true
| lexLess (op <) (x::xs) (y::ys) = let fun isEqual x y = if (not(x<y) andalso not(y<x)) then true else false 
in if (isEqual x y) then (lexLess (op <) xs ys) 
else if (x<y) then true else false
end

(* Answer 8 *)
exception myException
fun sortTreeList (op <) [] = []
| sortTreeList (op <) [x] = [x]
| sortTreeList (op <) (x::xs) = let val (z::zs) = (sortTreeList (op <) xs)
fun insert [] = [x] | insert (z::zs) = if (lexLess (op <) (Labels inorder x) (Labels inorder z)) then (x::(z::zs)) else (z::(insert zs))
in insert (z::zs)
end