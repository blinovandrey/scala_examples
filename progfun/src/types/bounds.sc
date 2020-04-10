trait Thing {

}
trait Vehicle extends Thing {

}
class Car extends Vehicle {

}
class Jeep extends Car {

}
class Motorcycle extends Vehicle {

}
class Vegetable extends Thing {

}

class F50 extends Jeep {

}

class Parking[A >: Jeep <: Vehicle](place1: A)

val p = new Parking(new Vegetable )