signature ALGORITHM =
sig
    type problem
    type solution

    val index   : int -> problem -> int
    val problem : int -> int -> problem

    val step : int -> problem -> (problem -> solution) -> solution
end

functor DP (A: ALGORITHM) =
struct
    type solution = A.solution

    fun step (s : int) (i : int) : solution =
        (A.step s) (A.problem s i)

    fun solve (size : int) : solution =
      let
        val t = Vector.tabulate (size, (step size))
      in
        Vector.sub (t, Vector.length t)
      end
end

structure MatrixChain :> ALGORITHM =
struct
    type problem = int * int
    type solution = int * (int * int) * (int vector)

    fun index n (x,y) = 0

    fun problem n i = (0,0)

    fun step (x,y) get = (0,(0,0),Vector.fromList [])
end

structure EditDistance :> ALGORITHM =
struct
  type problem = int * int
  type solution = int

  fun index n (x,y) = x * n + y

  fun problem n i = (i / n, i % n)
end
