

cbinom:=proc(n,k) n!/k!/(n-k)!end proc:

sumpartpiestim:=proc(n) sumpartpiestim(n):=sumpartpiestim(n-1)+cbinom(2*n,n)/4^(n-1)/(2*n+1) end proc:

sumpartpiestim(0):=4:

piestim:=proc(q,j) 1/q-sumpartpiestim(j)/2/(j+1) end proc:

pirationals:=proc(minnum,maxnum)
		local lobound, upbound, Listrat, n, j, q, evlobound, evpiestimn, evupbound, evpiestimq, k;
		if maxnum<6 then 
			return []
		end if;
		lobound:=cbinom(2*(n+1),n+1)/2/(n+1)/4^n;
		upbound:=cbinom(2*(n+1),n+1)/(2*n+1)/4^n;
		Listrat:=[];
		for j from max(minnum,6) to maxnum do
			evlobound:=eval(lobound,n=j);
			evpiestimn:=piestim(q,j);
			evupbound:=eval(upbound,n=j);
			k:=floor(j/3);
			evpiestimq:=eval(evpiestimn,q=k);
			while evpiestimq<=evupbound and k>1 do
				if evpiestimq>=evlobound then
					Listrat:=[op(Listrat),[j+1,k]];
				end if;
				k:=k-1;
				evpiestimq:=eval(evpiestimn,q=k)
			end do
		end do;
		Listrat
	end proc:
	
# Code for pi+e

sumparteestim:=proc(n) sumparteestim(n):=sumparteestim(n-1)+1/n! end proc:

sumparteestim(0):=1:

pipluseestim:=proc(q,j) 1/q-(sumpartpiestim(j)/2+sumparteestim(j))/(j+1) end proc:

pipluserationals:=proc(minnum,maxnum)
		local lobound, upbound, Listrat, n, j, q, evlobound, evpipluseestimj, evupbound, evpipluseestimq, k;
		if maxnum<10 then
			return []
		end if;
		lobound:=cbinom(2*(n+1),n+1)/2/(n+1)/4^n;
		upbound:=cbinom(2*(n+1),n+1)/(2*n+1)/4^n+1/(n+1)!/n;
		Listrat:=[];
		for j from max(minnum,10) to maxnum do
			evlobound:=eval(lobound,n=j);
			evpipluseestimj:=pipluseestim(q,j);
			evupbound:=eval(upbound,n=j);
			k:=floor(j/5);
			evpipluseestimq:=eval(evpipluseestimj,q=k);
			while evpipluseestimq<=evupbound and k>1 do
				if evpipluseestimq>=evlobound then
					Listrat:=[op(Listrat),[j+1,k]]
				end if;
				k:=k-1;
				evpipluseestimq:=eval(evpipluseestimj,q=k)
			end do
		end do;
		Listrat
	end proc: