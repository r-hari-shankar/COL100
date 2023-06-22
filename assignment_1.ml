(*RECURSIVE FACTORIAL*)
fun factorial (n) =
if n = 0 then 1
else n * factorial (n-1);

(*RECURSIVE POWER*)
fun pow (x, n) =
if n = 0 then 1
else x * pow (x, n-1);

(*RECURSIVE FIBONACCI*) 
fun fib (n) =
if (n=1) orelse (n=2) then 1
else fib (n-1) + fib (n-2);

(*INTEGER SQUARE ROOT*)
fun sqrt(n)=
if n=0 then 0
else
let val a=2*sqrt(n/4)
in let val b=a+1
in if b*b<n then b else a
end
end;

(*PERFECT NUMBER*)
fun perfect (n) =
   let fun add_factors (n) =
     let fun f (i) =
        if n mod i = 0 then i
        else 0;
      fun sum (a, b) =
        if a > b then 0
        else f(b) + sum (a, b-1);
     in sum (1, n div 2)
     end;
 in n = add_factors (n)
end;

(*ITERATIVE FACTORIAL*)
fun ifact (n) =
let fun fact_iter (m, f, c) =
  if c=m then f
  else fact_iter (m, f*(c+1), c+1);
in fact_iter (n, 1, 0)
end;

(*ITERATIVE FIBONACCI*)
fun ifib (n) =
let fun fib_iter (n, a, b, c) =
    if c = n then a+b
    else fib_iter (n, b, a+b, c+1);
in if n<=2 then 1
    else fib_iter (n, 1, 1,3)
end;

(*ITERATIVE POWER FUNCTION*)
fun ipow(x,n)=
let fun piter(x,n,c)=
if n=0 then c
else piter(x,n-1,c*x)
in piter(x,n,1)
end;