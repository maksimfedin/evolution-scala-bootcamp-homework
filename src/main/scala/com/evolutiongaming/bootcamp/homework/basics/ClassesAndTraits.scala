package com.evolutiongaming.bootcamp.homework.basics

/**
 * @author Maxim Fedin
 */

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.
object ClassesAndTraits extends App {

    sealed trait Shape2D extends Located2D
      with Bounded2D
      with Movable2D {
        def area: Double
    }

    sealed trait Located2D {
        def x: Double

        def y: Double
    }

    sealed trait Bounded2D {
        def minX: Double

        def maxX: Double

        def minY: Double

        def maxY: Double
    }

    sealed trait Movable2D {
        def move(dx: Double, dy: Double): Movable2D
    }

    final case class Point(x: Double, y: Double) extends Shape2D {
        override def minX: Double = x

        override def maxX: Double = x

        override def minY: Double = y

        override def maxY: Double = y

        override def move(dx: Double, dy: Double): Shape2D = Point(x + dx, y + dy)

        override def area: Double = 0
    }

    final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
        override def x: Double = centerX

        override def y: Double = centerY

        override def minX: Double = centerX - radius

        override def maxX: Double = centerX + radius

        override def minY: Double = centerY - radius

        override def maxY: Double = centerY + radius

        override def move(dx: Double, dy: Double): Shape2D = this.copy(centerX + dx, centerY + dy)

        override def area: Double = Math.PI * radius * radius
    }


    final case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape2D {
        override def x: Double = centerX

        override def y: Double = centerY

        override def minX: Double = centerX - width / 2

        override def maxX: Double = centerX + width / 2

        override def minY: Double = centerY - height / 2

        override def maxY: Double = centerY + height / 2

        override def move(dx: Double, dy: Double): Shape2D = this.copy(centerX + dx, centerY + dy)

        override def area: Double = width * height
    }

    final case class Square(centerX: Double, centerY: Double, side: Double) extends Shape2D {

        override def x: Double = centerX

        override def y: Double = centerY

        override def minX: Double = centerX - side / 2

        override def maxX: Double = centerX + side / 2

        override def minY: Double = centerY - side / 2

        override def maxY: Double = centerY + side / 2

        override def move(dx: Double, dy: Double): Shape2D = this.copy(centerX + dx, centerX + dy)

        override def area: Double = side * side
    }

    final case class Triangle(a: Point, b: Point, c: Point) extends Shape2D {

        override def x: Double = (a.x + b.x + c.x) / 3

        override def y: Double = (a.y + b.y + c.y) / 3

        override def minX: Double = List(a.x, b.x, c.x).min

        override def maxX: Double = List(a.x, b.x, c.x).max

        override def minY: Double = List(a.y, b.y, c.y).min

        override def maxY: Double = List(a.y, b.y, c.y).max

        override def move(dx: Double, dy: Double): Shape2D = this.copy(
            a = Point(x = a.x + dx, y = a.x + dy),
            b = Point(x = b.x + dx, y = b.x + dy),
            c = Point(x = c.x + dx, y = c.x + dy)
        )

        override def area: Double = Math.abs(0.5 * ((a.x - c.x) * (b.y - c.y) - (a.y - c.y) * (b.x - c.x)))

    }


    // 3D
    sealed trait Shape3D extends Located3D
      with Bounded3D
      with Movable3D {
        def surfaceArea: Double

        def volume: Double

    }

    sealed trait Located3D {
        def x: Double

        def y: Double

        def z: Double
    }

    sealed trait Bounded3D {
        def minX: Double

        def maxX: Double

        def minY: Double

        def maxY: Double

        def minZ: Double

        def maxZ: Double
    }

    sealed trait Movable3D {
        def move(dx: Double, dy: Double, dz: Double): Movable3D
    }


    object Origin3D extends Located3D {
        override def x: Double = 0

        override def y: Double = 0

        override def z: Double = 0

    }


    final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
        override def minX: Double = x

        override def maxX: Double = x

        override def minY: Double = y

        override def maxY: Double = y

        override def minZ: Double = z

        override def maxZ: Double = z

        override def move(dx: Double, dy: Double, dz: Double): Shape3D = Point3D(x + dx, x + dy, z + dz)

        override def surfaceArea: Double = 0

        override def volume: Double = 0
    }


    final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
        override def x: Double = centerX

        override def y: Double = centerY

        override def z: Double = centerZ

        override def minX: Double = centerX - radius

        override def maxX: Double = centerX + radius

        override def minY: Double = centerY - radius

        override def maxY: Double = centerY + radius

        override def minZ: Double = centerZ - radius

        override def maxZ: Double = centerZ + radius

        override def move(dx: Double, dy: Double, dz: Double): Shape3D = this.copy(centerX + dx, centerY + dy, centerZ + dz)

        override def surfaceArea: Double = 4 * Math.PI * radius * radius

        override def volume: Double = 4 / 3 * Math.PI * radius * radius * radius
    }


    final case class Cube(centerX: Double, centerY: Double, centerZ: Double, side: Double) extends Shape3D {

        override def x: Double = centerX

        override def y: Double = centerY

        override def z: Double = centerZ

        override def minX: Double = centerX - side / 2

        override def maxX: Double = centerX + side / 2

        override def minY: Double = centerY - side / 2

        override def maxY: Double = centerY + side / 2

        override def minZ: Double = centerZ + side / 2

        override def maxZ: Double = centerZ - side / 2

        override def move(dx: Double, dy: Double, dz: Double): Shape3D = this.copy(centerX + dx, centerX + dy, centerZ + dz)

        override def surfaceArea: Double = 6 * side * side

        override def volume: Double = side * side * side

    }

    final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, width: Double, height: Double, depth: Double) extends Shape3D {
        override def x: Double = centerX

        override def y: Double = centerY

        override def z: Double = centerZ

        override def minX: Double = centerX - width / 2

        override def maxX: Double = centerX + width / 2

        override def minY: Double = centerY - height / 2

        override def maxY: Double = centerY + height / 2

        override def minZ: Double = centerZ + depth / 2

        override def maxZ: Double = centerZ - depth / 2

        override def move(dx: Double, dy: Double, dz: Double): Shape3D = this.copy(centerX + dx, centerY + dy, centerZ + dz)

        override def surfaceArea: Double = 2 * (width * height + width * depth + height * depth)

        override def volume: Double = width * height * depth

    }


    final case class Triangle3D(a: Point3D, b: Point3D, c: Point3D) extends Shape3D {

        override def x: Double = (a.x + b.x + c.x) / 3

        override def y: Double = (a.y + b.y + c.y) / 3

        override def z: Double = (a.z + b.z + c.z) / 3


        override def minX: Double = List(a.x, b.x, c.x).min

        override def maxX: Double = List(a.x, b.x, c.x).max

        override def minY: Double = List(a.y, b.y, c.y).min

        override def maxY: Double = List(a.y, b.y, c.y).max

        override def minZ: Double = List(a.z, b.z, c.z).min

        override def maxZ: Double = List(a.z, b.z, c.z).max

        override def move(dx: Double, dy: Double, dz: Double): Shape3D = this.copy(
            a = Point3D(x = a.x + dx, y = a.y + dy, z = a.z + dz),
            b = Point3D(x = b.x + dx, y = b.y + dy, z = b.z + dz),
            c = Point3D(x = c.x + dx, y = c.y + dy, z = c.z + dz)
        )

        override def surfaceArea: Double = ???

        override def volume: Double = ???

    }

}
