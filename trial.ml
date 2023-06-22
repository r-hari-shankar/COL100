val condition=fn y =>fn(a,b)=>if ((2*b)>=y) then (2*a+1,2*b-y) else (2*a,2*b);
fun op1(a,(k,b))=(1*k,b+a);

fun accumulator2(a,n,oper1,oper2,iden)=
	if (n=0) then iden
	else if (n mod 2=0) then oper1(accumulator2(a,n div 2,oper1,oper2,iden))
	else oper2(a,oper1(accumulator2(a,n div 2,oper1,oper2,iden)));

fun divide(x,y)=accumulator2(1,x,condition y,op1,(0,0)); 