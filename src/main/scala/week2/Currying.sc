def mapReduce
(mapFunction: Int => Int)
(collectFunction: (Int, Int) => Int, unitValue: Int)
(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, collectFunction(acc, mapFunction(a)))
  }

  loop(a, unitValue)
}

def product(a: Int, b: Int): Int = mapReduce(x => x)((x, y) => x * y, 1)(a, b)
def sum(a: Int, b: Int): Int = mapReduce(x => x)((x, y) => x + y, 0)(a, b)
def factorial(a: Int): Int = product(2, a)

product(1, 3)
sum(1, 3)
factorial(13)

