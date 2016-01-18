abstract class Tree[+T]

case class Node[T](label: T, left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[T](label: T) extends Tree[T]

trait Addable[T] {
def +(t: T):T
}

class A(x: Int) extends Addable[A]{ 
val value = x
override def toString = "A(" + value + ")"
def +(t: A):A = new A(this.value + t.value)
}

class B(y: Int) extends A(y) { 
override val value = y
override def toString = "B(" + value + ")"
}

class C(z: Int) extends B(z){ 
override val value = z
override def toString = "C(" + value + ")"
}

object Part2{
def inOrder[T](t: Tree[T]): List[T] = t match {
case Leaf(label) => List(label)
case Node(label,left,right) => inOrder(left) ++ List(label) ++ inOrder(right)
}

def treeSum[T <: Addable[T]](t:Tree[T]): T = t match {
case Leaf(label) => label
case Node(label,left,right) => treeSum(left) + label + treeSum(right)
}

def treeMap[T,V](f: T=>V, t: Tree[T]): Tree[V] = t match {
case Leaf(label) => Leaf(f(label))
case Node(label,left,right) => 
Node(f(label),treeMap(f,left),treeMap(f,right))
}

def BTreeMap(f: B=>B, t: Tree[B]): Tree[B] = t match {
case Leaf(label) => Leaf(f(label))
case Node(label,left,right) => 
Node(f(label), BTreeMap(f,left),BTreeMap(f,right))
} 

def test(){
def faa(a:A):A = new A(a.value+10)
def fab(a:A):B = new B(a.value+20)
def fba(b:B):A = new A(b.value+30)
def fbb(b:B):B = new B(b.value+40)
def fbc(b:B):C = new C(b.value+50)
def fcb(c:C):B = new B(c.value+60)
def fcc(c:C):C = new C(c.value+70)
def fac(a:A):C = new C(a.value+80)
def fca(c:C):A = new A(c.value+90)

val myBTree: Tree[B] = Node(new B(4),Node(new B(2),Leaf(new B(1)),Leaf(new B(3))), Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))
val myATree: Tree[A] = myBTree

println("inOrder = " + inOrder(myATree))
println("Sum = " + treeSum(myATree))

//println(BTreeMap(faa,myBTree)) -- function subtyping is contravariant on input type and covariant on return type. faa, here, is a function from A => A. Thus, passing faa and a tree of type Tree[B] as parameters to a function accepting a function of type B=>B and a tree of type Tree[B] gives a compile-time type error since function subtyping is covariant on the return type and thus, A => A is not a subtype of B => B.

println(BTreeMap(fab,myBTree))

//println(BTreeMap(fba,myBTree)) -- function subtyping is contravariant on input type and covariant on return type. fba, here, is a function of type B => A. Thus, passing fba and a tree of type Tree[B] as parameters to a function accepting a function of type B=>B and a tree of type Tree[B] gives a compile-time type error since function subtyping is covariant on the return type and thus, B => A is not a subtype of B => B.

println(BTreeMap(fbb,myBTree))
println(BTreeMap(fbc,myBTree))

//println(BTreeMap(fcb,myBTree)) -- function subtyping is contravariant on input type and covariant on return type. fcb, here, is a function of type C => B. Thus, trying to pass fcb and a tree of type Tree[B] as parameters to a function accepting a function of type B=>B and a tree of type Tree[B] gives a compile-time type error since function subtyping is contravariant on the input type and thus, C => B is not a subtype of B => B.

//println(BTreeMap(fcc,myBTree)) -- function subtyping is contravariant on input type and covariant on return type. fcc, here, is a function of type C => C. Thus, passing fcc and a tree of type Tree[B] as parameters to a function accepting a function of type B=>B and a tree of type Tree[B] gives a compile-time type error since function subtyping is contravariant on the input type and thus, C => C is not a subtype of B => B.

println(BTreeMap(fac,myBTree))

//println(BTreeMap(fca,myBTree)) -- fca, here, is a function of type C => A. Thus, trying to pass fca and a tree of type Tree[B] as parameters to a function accepting a function of type B=>B and a tree of type Tree[B] gives a compile-time type error since function subtyping is contravariant on the input type and covariant on the return type and thus, C => A is not a subtype of B => B.

println(treeMap(faa,myATree))
println(treeMap(fab,myATree))

//println(treeMap(fba,myATree)) -- function subtyping is contravariant on input type and covariant on return type. fba, here, is a function of type B => A. Thus, passing fba and a tree of type Tree[A] as parameters to a generic function accepting a function of type T=>V and a tree of type Tree[T] gives a compile-time type error since function subtyping is contravariant on the input type and thus, B => A is not a subtype of A => A.

//println(treeMap(fbb,myATree)) -- function subtyping is contravariant on input type and covariant on return type. fbb, here, is a function from B => B. Thus, passing fbb and a tree of type Tree[A] as parameters to a generic function accepting a function of type T=>V and a tree of type Tree[T] gives a compile-time type error since function subtyping is contravariant on the input type and thus, B => B is not a subtype of A => B.

//println(treeMap(fbc,myATree)) -- function subtyping is contravariant on input type and covariant on return type. fbc, here, is a function of type B => C. Thus, passing fbc and a tree of type Tree[A] as parameters to a generic function accepting a function of type T=>V and a tree of type Tree[T] gives a compile-time type error since function subtyping is contravariant on the input type and thus, B => C is not a subtype of A => C.

//println(treeMap(fcb,myATree)) -- function subtyping is contravariant on input type and covariant on return type. fcb, here, is a function of type C => B. Thus, passing fcb and a tree of type Tree[A] as parameters to a generic function accepting a function of type T=>V and a tree of type Tree[T] gives a compile-time type error since function subtyping is contravariant on the input type and thus, C => B is not a subtype of A => B.

//println(treeMap(fcc,myATree)) -- function subtyping is contravariant on input type and covariant on return type. fcc, here, is a function of type C => C. Thus, passing fcc and a tree of type Tree[A] as parameters to a generic function accepting a function from T=>V and a tree of type Tree[T] gives a compile-time type error since function subtyping is contravariant on the input type and thus, C => C is not a subtype of A => C.

println(treeMap(fac,myATree))

//println(treeMap(fca,myATree)) -- function subtyping is contravariant on input type and covariant on return type. fca, here, is a function of type C => A. Thus, passing fca and a tree of type Tree[A] as parameters to a generic function accepting a function of type T=>V and a tree of type Tree[T] gives a compile-time type error since function subtyping is contravariant on the input type and thus, C => A is not a subtype of A => A.

}

def main(args: Array[String]){
test()
}
}