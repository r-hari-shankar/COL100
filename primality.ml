(* PRIMALITY TEST *)

(*METHOD 1)

fun prime(n) =
	let
		fun smallest_divisor(n) =
			let
					fun find_divisor(n,test_divisor) =
						let
							fun square(x:int) = x*x
						in
							if square(test_divisor) > n then n
							else if (n mod test_divisor) = 0 then test_divisor
							else
								find_divisor(n, test_divisor + 2)
						end
			in
				find_divisor(n,3)
			end;
		fun odd(n) = (n mod 2 = 1)
	in
		(n=2) orelse (odd(n) andalso (n = smallest_divisor(n)))
end;

(*METHOD 2*)

fun prime(n,q) =
	let
		fun prime_test(n,q,failed) =
			let
				fun Fermat_test(n) =
					let
						fun expmod(b,e,m) =
							let
								fun sqr(x) = x*x : int;
							in
								if e = 0 then 1
								else if (e mod 2) = 0 then sqr(expmod(b,e div 2,m)) mod m
								else ((b mod m) * expmod(b,e-1,m)) mod m
							end
					in
						let
							val a = Rand.randint(n-2) + 2
						in
							(a = expmod(a,n,n))
						end
					end
			in
				if q = 0 then true
				else if failed then false
				else prime_test(n,q-1,not(Fermat_test(n)))
			end
	in
		if (n = 2) then true
		else prime_test(n,q,false)
	end;