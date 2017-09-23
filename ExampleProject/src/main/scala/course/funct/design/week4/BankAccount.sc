
abstract class AnimalCounter
{
  var animals = 0

  def name: String

  def count()
  {
    animals += 1
    println("%d %ss created so far".format(animals, name))
  }
}

abstract class Animal
{
  def companion: AnimalCounter
  companion.count()
}

object Dog extends AnimalCounter
{
  val name = "dog"
}

class Dog extends Animal
{
  def companion = Dog
}

object Cat extends AnimalCounter
{
  val name = "cat"
}

class Cat extends Animal
{
  def companion = Cat
}


