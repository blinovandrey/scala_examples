trait Person {}
class Grandparent extends Person {}
class Parent extends Grandparent {}
class Child extends Parent {}
class Grandchild extends Child {}

class VariantBox[A] {}
class CoBox[+A] {}
class ContraBox[-A] {}

// Variant

def foo1(p: VariantBox[Person]): VariantBox[Person] = identity(p)

foo1(new VariantBox[Person])

// Invariant

//fool(new VariantBox[Parent])

// Covarinat

def foo2(p: CoBox[Person]): CoBox[Person] = identity(p)

foo2(new CoBox[Child])

def foo3(p: CoBox[Parent]): CoBox[Parent] = identity(p)

foo3(new CoBox[Grandchild])

//foo3(new CoBox[Grandparent])


// Contravariant

def foo4(p: ContraBox[Parent]): ContraBox[Parent] = identity(p)

foo4(new ContraBox[Grandparent])
