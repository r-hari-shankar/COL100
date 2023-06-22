(victor banerjee)

(ASSIGNMENT 4)

(question 1)

fun accumulator(a, b, f, succ, oper, iden)=
if (a>b) then iden
else oper( f(a), accumulator(succ(a), b, f, succ, oper, iden));

(a)
fun perfect(n)=
let
fun f(i) = if (n mod i=0) then i
else 0
fun next(x) = x+1
in n=accumulator(1,n div 2,f,next,op+,0)
end;

(b)
fun exp(x,n)=
if (n=0) then 1
else
let fun term(n) = if (n=1) then 1
else term(n-1)*(x div (n-1))
fun next(n) = n+1
in accumulator(1,n,term,next,op+,0)
end;

(c)
fun accumulator(a, b, f, succ, oper, iden)=
if (b=0) then iden
else if (b mod 2 =0) then 
accumulator(f(a), succ(b), f, succ, oper, iden)
else oper(a, accumulator(f(a), succ(b), f, succ, oper, iden));

fun fast_power(x,n)=
let
fun term(u)= u*u
fun next(u)= u div 2
in
accumulator(x,n,term,next,op*,1)
end;


(question 2)

fun sum(a, b, f, succ, oper, iden)=
if (a>b) then iden
else oper( f(a), sum(succ(a), b, f, succ, oper, iden));

fun doublesum(a,b,c,d)=
let fun subsum(c,d,i)=
let
fun f(j)= i+1
fun succ(j)= j+1
in sum(c,d,f,succ,op+,0)
end;
fun f(i)= subsum(c,d,i)
fun next(i)= i+1
in sum(a,b,f,next,op+,0)
end;


(question 3)

signature Interval =
	  sig
	  type interval;
	  val make_interval: real*real -> interval;
	  val intadd: interval*interval->interval;
	  val intsubtract: interval*interval->interval;
	  val intmult: interval*interval->interval;
	  val intdiv: interval*interval->interval;
	  end;
structure Interval=
	struct
	datatype interval=maker of real*real;
	fun make_interval(p:real,q:real):interval= maker (p,q);
	fun lb(maker(x,_))=x;
	fun ub(maker(_,y))=y;
	fun intadd(a,b)=
	let 
	val x=lb(a)+lb(b);
	val y=ub(a)+ub(b);
	in 
	make_interval(x,y)
	end;
	fun intsubtract(a,b)=
	let 
	val x=lb(a)-lb(b);
	val y=ub(a)-ub(b);
         in 
         make_interval(x,y)
	end;
	fun intmult(a,b)=
	let 
	val x=lb(a)*lb(b);
	val y=ub(a)*ub(b);
	in 
	make_interval(x,y)
	end;
	fun intdiv(a,b)= 
	let
	val x=lb(a)*(1.0/lb(b));
	val y=lb(a)*(1.0/lb(b));
	in 
        make_interval(x,y)
	end;
	end;



(question 4)

fun accumulator(a, b, f, succ, oper, iden)=
if (b=0) then iden
else if (b mod 2 =0) then 
accumulator(f(a), succ(b), f, succ, oper, iden)
else oper(a, accumulator(f(a), succ(b), f, succ, oper, iden));

(a)
fun fast_power(x,n)=
let
fun term(u)= u*u
fun next(u)= u div 2
in
accumulator(x,n,term,next,op*,1)
end;

(b)
fun multiply(x,y)=
let
fun term(u)= 2*u
fun next(u)= u div 2
in
accumulator(x,y,term,next,op+,0)
end;