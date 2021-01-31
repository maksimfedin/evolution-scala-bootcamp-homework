package com.evolutiongaming.bootcamp.homework.basics

import com.evolutiongaming.bootcamp.homework.basics.ClassesAndTraits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

/**
 * @author Maxim Fedin
 */
class ClassesAndTraitsTest extends AnyFlatSpec {


    // 2D
    "Point" should "be ok" in {
        val point = Point(x = 3.0, y = 2.0)
        point.minX shouldEqual 3.0
        point.maxX shouldEqual 3.0
        point.minY shouldEqual 2.0
        point.maxY shouldEqual 2.0
        point.move(15, 20) shouldEqual Point(18.0, 22.0)
        point.move(-1, -10) shouldEqual Point(2.0, -8.0)
    }


    "Circle" should "be ok" in {
        val radius = 10.0
        val circle = Circle(centerX = 1.0, centerY = 0.0, radius = radius)
        circle.minX shouldEqual -9.0
        circle.maxX shouldEqual 11.0
        circle.minY shouldEqual -10.0
        circle.maxY shouldEqual 10.0
        circle.move(10, 10) shouldEqual Circle(11.0, 10.0, 10.0)
        circle.move(-1, -10) shouldEqual Circle(0.0, -10.0, 10.0)
        circle.area shouldEqual Math.PI * radius * radius

    }


    "Rectangle" should "be ok" in {
        val rectangle = Rectangle(centerX = 1.0, centerY = 0.0, width = 4.0, height = 5.0)
        rectangle.minX shouldEqual -1.0
        rectangle.maxX shouldEqual 3.0
        rectangle.minY shouldEqual -2.5
        rectangle.maxY shouldEqual 2.5
        rectangle.move(10, 10) shouldEqual Rectangle(11.0, 10.0, 4.0, 5.0)
        rectangle.move(-1, -10) shouldEqual Rectangle(0.0, -10.0, 4.0, 5.0)
        rectangle.area shouldEqual 20.0
    }


    "Square" should "be ok" in {
        val square = Square(centerX = 1.0, centerY = 0.0, side = 10.0)
        square.minX shouldEqual -4.0
        square.maxX shouldEqual 6.0
        square.minY shouldEqual -5.0
        square.maxY shouldEqual 5.0
        square.move(10, 10) shouldEqual Square(11.0, 11.0, 10.0)
        square.move(-1, -10) shouldEqual Square(0.0, -9.0, 10.0)
        square.area shouldEqual 100.0
    }

    "Triangle" should "be ok" in {
        val triangle = Triangle(a = Point(3, 2), b = Point(7, 5), c = Point(0, 0))
        triangle.minX shouldEqual 0.0
        triangle.maxX shouldEqual 7.0
        triangle.minY shouldEqual 0.0
        triangle.maxY shouldEqual 5.0
        triangle.move(10, 10) shouldEqual Triangle(Point(13.0, 13.0), Point(17.0, 17.0), Point(10.0, 10.0))
        triangle.move(-1, -10) shouldEqual Triangle(Point(2.0, -7.0), Point(6.0, -3.0), Point(-1.0, -10.0))
        triangle.area shouldEqual 0.5

    }

    // 3D
    "3D Point" should "be ok" in {
        val point = Point3D(x = 3.0, y = 2.0, z = 1.0)
        point.minX shouldEqual 3.0
        point.maxX shouldEqual 3.0
        point.minY shouldEqual 2.0
        point.maxY shouldEqual 2.0
        point.minZ shouldEqual 1.0
        point.maxZ shouldEqual 1.0
        point.move(15, 20, 10) shouldEqual Point3D(18.0, 23.0, 11.0)
        point.move(-1, -10, -5) shouldEqual Point3D(2.0, -7.0, -4.0)
    }


    "Sphere" should "be ok" in {
        val radius = 10.0
        val sphere = Sphere(centerX = 1.0, centerY = 0.0, centerZ = 2.0, radius = radius)
        sphere.minX shouldEqual -9.0
        sphere.maxX shouldEqual 11.0
        sphere.minY shouldEqual -10.0
        sphere.maxY shouldEqual 10.0
        sphere.minZ shouldEqual -8.0
        sphere.maxZ shouldEqual 12.0
        sphere.move(10, 10, 3) shouldEqual Sphere(11.0, 10.0, 5.0, 10.0)
        sphere.move(-1, -10, 5) shouldEqual Sphere(0.0, -10.0, 7.0, 10.0)
        sphere.surfaceArea shouldEqual 4 * Math.PI * radius * radius
        sphere.volume shouldEqual 4 / 3 * Math.PI * radius * radius * radius
    }

    "Cube" should "be ok" in {
        val sphere = Cube(centerX = 1.0, centerY = 0.0, centerZ = 2.0, side = 20.0)
        sphere.minX shouldEqual -9.0
        sphere.maxX shouldEqual 11.0
        sphere.minY shouldEqual -10.0
        sphere.maxY shouldEqual 10.0
        sphere.minZ shouldEqual 12.0
        sphere.maxZ shouldEqual -8.0
        sphere.move(10, 10, 3) shouldEqual Cube(11.0, 11.0, 5.0, 20.0)
        sphere.move(-1, -10, 5) shouldEqual Cube(0.0, -9.0, 7.0, 20.0)
        sphere.surfaceArea shouldEqual 2400.0
        sphere.volume shouldEqual 8000.0
    }


    "Cuboid" should "be ok" in {
        val cuboid = Cuboid(centerX = 1.0, centerY = 0.0, centerZ = 2.0, width = 20.0, height = 10.0, depth = 30.0)
        cuboid.minX shouldEqual -9.0
        cuboid.maxX shouldEqual 11.0
        cuboid.minY shouldEqual -5.0
        cuboid.maxY shouldEqual 5.0
        cuboid.minZ shouldEqual 17.0
        cuboid.maxZ shouldEqual -13.0
        cuboid.move(10, 10, 3) shouldEqual Cuboid(11.0, 10.0, 5.0, 20.0, 10.0, 30.0)
        cuboid.move(-1, -10, 5) shouldEqual Cuboid(0.0, -10.0, 7.0, 20.0, 10.0, 30.0)
        cuboid.surfaceArea shouldEqual 2200.0
        cuboid.volume shouldEqual 6000.0
    }


    "Triangle3D" should "be ok" in {
        val triangle = Triangle3D(a = Point3D(3, 2, 1), b = Point3D(7, 5, 9), c = Point3D(0, 0, 0))
        triangle.minX shouldEqual 0.0
        triangle.maxX shouldEqual 7.0
        triangle.minY shouldEqual 0.0
        triangle.maxY shouldEqual 5.0
        triangle.minZ shouldEqual 0.0
        triangle.maxZ shouldEqual 9.0
        triangle.move(10, 10, 3) shouldEqual Triangle3D(Point3D(13.0, 12.0, 4.0), Point3D(17.0, 15.0, 12.0), Point3D(10.0, 10.0, 3.0))
        triangle.move(-1, -10, 5) shouldEqual Triangle3D(Point3D(2.0, -8.0, 6.0), Point3D(6.0, -5.0, 14.0), Point3D(-1.0, -10.0, 5.0))

    }

}
