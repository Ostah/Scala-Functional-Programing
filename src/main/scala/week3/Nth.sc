import week3._



  val test: List[String] = new week3.Cons[String]("1", new Cons[String]("2", new Nil[String]()))

  def nth[T](list: List[T], i : Int) : T = {
    if(i < 0) throw new IndexOutOfBoundsException();

    def dig(cons: List[T], element: Int): T = {
       if(cons.isEmpty) throw new IndexOutOfBoundsException();
       if (element == i) return cons.head
       else dig(cons.tail, element +1)
    }

    dig(list, 0)
  }

  nth(test, -1)
