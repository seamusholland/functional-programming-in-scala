object MyModule {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, p1: Int, p2: Int): Int = 
            if (n == 0) p2
            else go(n-1, p2, p1+p2)

        if (n <= 0) 0 
        else go(n, 0, 1)
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def go(n: Int): Boolean = 
            if (n == as.length) true
            else if (n > 0 && !ordered(as(n-1), as(n))) false
            else go(n+1)

        go(0)
    }

    def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
        (a) => (b) => f(a, b)
    }

    def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
        (a, b) => f(a)(b)
    }

    def compose[A,B,C](f: B => C, g: A => B): A => C = {
        (a) => f(g(a))
    }
}   