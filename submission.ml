(*COL 100 ASSIGNMENT 2*)

(*MADE BY:R HARI SHANKAR:2019CS10386 *)

(*MINOR 1 SOLUTION*)

(*PROBLEM 1*)

(*PART 1*)

(*invariant is (0<=i<=a)and(sum=i*b) *)
fun mult(a,b)=
	if (b=0) then 0 
	else
		let
			fun imult(a,b,i)=
				if (i=a) then 0
				else add(imult(a,b,add(i,1)),b)
				
	in
		imult(a,b,0)
	end;

(*PART 2:SOLVED IN EXAM PAPER*)

(*PART 3*)

fun fastmult(a,b) =
	if (b = 0) then 0
	else if even(b) then
		double(fastmult(a,halve(b)))
	else add(a,fastmult(a,dec(b)));

(*TIME COMPLEXITY FOUND IN EXAM*)

(*PROBLEM 2*)

fun rook(p,q) =
	if p = 0 orelse q = 0 then 0
	else if p =1 andalso q = 1 then 1
		else rook(p-1, q) + rook(p, q-1);

(*PROBLEM 3 DONE ON A SHEET*)

(*PROBLEM 4*)

fun maxpow(n) =
	if (n div 4 = 0) then 1
	else 4*maxpow(n div 4);

fun intsqrt(n) =
	let fun maxpow(n) =
		if (n div 4 = 0) then 1
		else 4*maxpow(n div 4);
	    fun iter(i,n,c) =
		if (c = 0) then i
		else 
			if (2*i+1)*(2*i+1) > (n div c) then iter(2*i,n,c div 4)
			else iter(2*i+1,n,c div 4)
in
	iter(0,n,maxpow(n))
end;

(*TIME COMPLEXITY FOUND IN PAPER*)

(*BONUS QUESTION SUBMITTED ON MOODLE*)