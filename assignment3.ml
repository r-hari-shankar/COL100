(*assignment2*)

(*Q1*)
fun acc(a,b,f1,f2,check1,check2,ope,id,suca,sucb)=
	if check1(a,b) then id
	else if check2(a,b) then ope(f1(a,b),acc(suca(a),sucb(b),f1,f2,check1,check2,ope,id,suca,sucb))
	else ope(f2(a,b),acc(suca(a),sucb(b),f1,f2,check1,check2,ope,id,suca,sucb))

fun fastpower(x,n)= acc(x,n,fn (x,y) => 1, fn (x,y) => x, fn (x,y) => (y = 0), fn (x,y) => (y mod 2 = 0), op*, 1, fn x => x*x, fn x => x div 2);

fun perfect(n)=
	let val num= acc(n,2,fn (x,y) => y ,fn (x,y) =>  if (x mod y = 0) then  y + (x div y) else 0 ,fn (x,y) => (x < (y*y)) ,fn (x,y) => (x= (y*y)), op+, 0, fn x => x, fn x=> x+1);
	in  (n=num+1)
	end;
	
fun epowerx(x,n)=acc(x,n,fn (x,y) => 0.0, fn (x,y)=> let fun cal(x,n) = if (n=0) then 1.0 else  (real(x)/real(n))*(cal(x,n-1)) ; in cal(x,y) end, fn (x,y) => y=0, fn (x,y)=> false, op+, 1.0, fn x=> x,fn x=> x-1) ;




(*Q2*)(*problem 6 on pg 105*)
fun acc2(a,b,c,d,i,j,f1,f2,comp,ope,id,suc)=
	if comp(i,b) then id
	else if comp(j,d) then ope(f1(i,j),acc2(a,b,c,d,suc(i),c,f1,f2,comp,ope,id,suc))
	else ope(f2(i,j),acc2(a,b,c,d,i,suc(j),f1,f2,comp,ope,id,suc))

fun doublesum(a,b,c,d,f)=acc2(a,b,c,d,a,c,fn (x,y)=> 0,fn (x,y) => f(x,y),op>,op+,0,fn x=>x+1); 
	


(*Q3*)(*problem 2 on pg 125*)
signature interval =
	sig
	type inter
	val make_interval : real * real -> inter 
	val intadd : inter*inter ->inter
	val intsubtract : inter*inter ->inter
	val intmult	: inter*inter ->inter
	val intdiv : inter*inter ->inter
	end;

structure INTERVALS=
	struct 
		datatype inter = intermaker of real*real
		fun make_interval(a :real , b:real) :inter = intermaker(a,b);
		fun lowerint(intermaker(a,_)) = a;
		fun upperint(intermaker(_,b)) = b;
		fun intadd(a,b) = intermaker(lowerint(a)+lowerint(b) , upperint(a)+upperint(b)) ;
		fun intsubtract(a,b) = intermaker(lowerint(a)-lowerint(b) , upperint(a)-upperint(b)) ;
		fun intmult(a,b) = intermaker(lowerint(a)*lowerint(b) , upperint(a)*upperint(b)) ;
		fun intdiv(a,b) = intermaker(lowerint(a) /lowerint(b) , upperint(a) /upperint(b)) ;
		end;

(*Q4*)
fun square(x)=x*x;
fun double(a)=2*a;
val condition=fn y =>fn(a,b)=>if y mod 2 = 1 andalso 2*b+1>=y then (2*a+1, 2*b+1-y) else if y mod 2 = 0 andalso 2*b>=y then (2*a+1, 2*b-y) else (2*a, 2*b);
fun op1(a,(k,b))=(k,b+a);
fun basicMul(a, b) = a*b;
fun basicAdd(a, b) = a+b;
fun accumulator2(a,n,oper1,oper2,iden)=
	if (n=0) then iden
	else if (n mod 2=0) then oper1(accumulator2(a,n div 2,oper1,oper2,iden))
	else oper2(a,oper1(accumulator2(a,n div 2,oper1,oper2,iden)));
fun fastpower(x,n)=accumulator2(x,n,square,basicMul,1);
fun multiply(a,b)=accumulator2(a,b,double,basicAdd,0);
fun divide(x,y)=accumulator2(1,x,condition y,op1,(0,0)); 