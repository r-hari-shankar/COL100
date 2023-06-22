(*COL100 ASSIGNMENT-4*)
(*MADE BY R HARI SHANKAR 2019CS10386*)

(*Q1*)

(*CODE FOR ACCUMULATOR*)
fun accumulator(a,b,con1,con2,fun1,fun2,oper,id,suc1,suc2)=
	if con1(a,b) then id
	else if con2(a,b) then oper(fun1(a,b),accumulator(suc1(a),suc2(b),con1,con2,fun1,fun2,oper,id,suc1,suc1))
	else oper(fun2(a,b),accumulator(suc1(a),suc2(b),con1,con2,fun1,fun2,oper,id,suc1,suc2));

(*CODE FOR FASTPOWER*)
fun fastpower(x,n)= 
	accumulator(x,n, fn (x,y) => (y = 0), fn (x,y) => (y mod 2 = 0), fn (x,y) => 1, fn (x,y) => x, op*, 1, fn x => x*x, fn x => x div 2);

(*CODE TO CHECK PERFECT NUMBER*)
fun perfect(n)=
	let val num= accumulator(n,2,fn (x,y) => (x < (y*y)) ,fn (x,y) => (x= (y*y)),fn (x,y) => y ,fn (x,y) =>  if (x mod y = 0) then  y + (x div y) else 0 , op+, 0, fn x => x, fn x=> x+1);
	in  (n=num+1)
	end;
	
(*CODE FOR e^x*)
fun ex(x,n)=
	accumulator(x,n,fn (x,y) => y=0, fn (x,y)=> false,fn (x,y) => 0.0, fn (x,y)=> let fun cal(x,n) = if (n=0) then 1.0 else  (real(x)/real(n))*(cal(x,n-1)) ; in cal(x,y) end, op+, 1.0, fn x=> x,fn x=> x-1) ;




(*Q2*)
fun summation(a, b, f, suc) = if a > b then 0
			      else
					f(a) + summation(suc(a), b, f, suc);

fun doubleSummation(a, b, c, d, f, suc1, suc2) = if a>b then 0
			     			else
							summation(c, d, f(a), suc1) + doubleSummation(suc2(a), b, c, d, f, suc1, suc2);
(*Q3*)
signature Interval =
	  sig
	  type interval;
	  val makeinterval: real*real -> interval;
	  val intadd: interval*interval->interval;
	  val intsubtract: interval*interval->interval;
	  val intmult: interval*interval->interval;
	  val intdiv: interval*interval->interval;
	  end;

structure INT1:Interval=
	struct
	datatype interval=construct of real*real;

	exception NotPossible;

	fun makeinterval(p:real,q:real):interval=
		if(p>q)then raise NotPossible
		else construct (p,q);
	fun lower_bound(construct(x,_))=x;
	fun upper_bound(construct(_,y))=y;
	fun intadd(a,b)=
		let 
			val x=lower_bound(a)+lower_bound(b);
			val y=upper_bound(a)+upper_bound(b);
		in 
			makeinterval(x,y)
		end;

	fun intsubtract(a,b)=
		let 
			val x=lower_bound(a)-lower_bound(b);
			val y=upper_bound(a)-upper_bound(b);
		in 
			if(x>y)then raise NotPossible
			else makeinterval(x,y)
		end;

	fun intmult(a,b)=
		let 
			val x=lower_bound(a)*lower_bound(b);
			val y=upper_bound(a)*upper_bound(b);
		in 
			makeinterval(x,y)
		end;

	fun intdiv(a,b)= 
		let
			val x=lower_bound(a)*(1.0/lower_bound(b));
			val y=lower_bound(a)*(1.0/lower_bound(b));
		in 
			if(x>y)then raise NotPossible
			else makeinterval(x,y)
		end;
	end;


(*Q4*)

fun square(x)=x*x;
fun double(a)=2*a;
val condition=fn y =>fn(a,b)=>if ((2*b)>=y) then (2*a+1,2*b-y) else (2*a,2*b);
fun op1(a,(k,b))=(k,b+a);
fun basicMul(a, b) = a*b;
fun basicAdd(a, b) = a+b;

(*CODE FOR ACCUMULATOR*)
fun accumulator2(a,n,oper1,oper2,iden)=
	if (n=0) then iden
	else if (n mod 2=0) then oper1(accumulator2(a,n div 2,oper1,oper2,iden))
	else oper2(a,oper1(accumulator2(a,n div 2,oper1,oper2,iden)));

(*CODE FOR FAST POWER*)
fun fastpower(x,n)=accumulator2(x,n,square,basicMul,1);

(*CODE FOR FAST MULTIPLICATION*)
fun multiply(a,b)=accumulator2(a,b,double,basicAdd,0);

(*CODE FOR DIVIDE*)(*MINOR 1 ALGO*)
fun divide(x,y)=accumulator2(1,x,condition y,op1,(0,0)); 
