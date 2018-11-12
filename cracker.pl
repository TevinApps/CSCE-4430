% Tevin Mosley
% CSCE 4430

% In order to run this program, open in the compiler and use "go(0)." to get the output.

  p(0,1,3).
  p(0,2,5).
  p(1,3,6).
  p(1,4,8).
  p(2,4,7).
  p(2,5,9).
  p(3,6,10).
  p(3,7,12).
  p(4,7,11).
  p(4,8,13).
  p(5,8,12).
  p(5,9,14).
  p(3,4,5).
  p(6,7,8).
  p(7,8,9).
  p(10,11,12).
  p(11,12,13).
  p(12,13,14).

  app([],[],[]).
  app([H|T],[S|P],[H,S|L]):- app(T,P,L).
  
  step(L):- findall((p(X,Y,Z)),p(X,Y,Z),L1),findall((p(Z,Y,X)),p(X,Y,Z),L2),app(L1,L2,L).


   init_([],0,N).
   init_([0|L],F,N):- K is F-1 ,K=N,init_(L,K,N),!.
   init_([1|L],F,N):- K is F-1 ,init_(L,K,N),!.
   init(I,[14|P]):- init_(L,15,I),reverse(L,P).


   find([V|T],I,V,C):- C = I.
   find([H|T],I,V,C):- S is C + 1 , find(T,I,V,S).

   set([H|T],I,V,[V|T],C):- C = I.
   set([H|T],I,V,[H|L],C):- K is C + 1,set(T,I,V,L,K).
   
   move([H|Ta],p(F,O,T),B):- K=H, D=Ta , findall((F,O,T),p(F,O,T),_),test(F,O,T,K,D,B),!.
   test(F,O,T,K,D,[Q|Y3]):- Q is K-1,find(D,F,1,0), find(D,O,1,0) , find(D,T,0,0),C=D,set(C,F,0,Y,0),set(Y,O,0,Y2,0),set(Y2,T,1,Y3,0).


    solve([H|Ls],[H|Ls]):- H <2 .
    solve([H|T],New):- H >= 2,step(L),check_([H|T],L,New).
    check_(L,[],[]).
    check_(Ls,[H|T],[H|New]):- move(Ls,H,Kc),solve(Kc,New).
    check_(Ls,[H|T],New):- check_(Ls,T,New).


    f_part([H|T],[H|K]):- \+ integer(H), f_part(T,K).
    f_part([H|T],[]):- integer(H).
    
    l_part([],[]).
    l_part([H|T],[H|K]):- integer(H), l_part(T,K).
    l_part([H|T],K):- \+ integer(H),l_part(T,K).
    
    try(S,K):- solve(S,P) , b(P,K).
    
    puzzle(I,Kd,Ms,Newkd):- init(I,Kd),solve(Kd,So),f_part(So,Ms),l_part(So,Newkd).

    writenum(0,P).
    writenum(N,P):- N >=1,T is N-1,write(P),writenum(T,P),!.
    


    
    show([H|T]):- Lines=[p(4,0,0),p(3,1,2),p(2,3,5),p(1,6,9),p(0,10,14)],test_(T,Lines).
    test_(D,[p(X,Y,Z)|T]):- findall((X,Y,Z),p(X,Y,Z),_),writenum(X,' '),range(D,Y,Z),test_(D,T).
    range(D,A,B):- A =< B,T is A+1,find(D,A,0,0),write('. '),range(D,T,B),!.
    range(D,A,B):- A=<B,T is A+1,write('x '),range(D,T,B).
    range(D,A,B):- A>B,nl,!.
    show([H|T]).
    
    
    
    replay([],Kd):- show(Kd).
    replay([p(F,O,T)|Ms],[H|Ta]):- show([H|Ta]), K is H,findall((F,O,T),p(F,O,T),_),set(Ta,F,0,Y1,0),set(Y1,O,0,Y2,0),set(Y2,T,1,Y,0),G is K -1 ,replay(Ms,[G|Y]).

     terse(15).
     terse(N):- N=<14, C is N+1,puzzle(N,Kd1,Ms,Kd2),write(Kd1),nl,loop(Ms,Kd2,C),!.
     loop([],Kd2,C):- terse(C),!.
     loop([M|Ms],Kd2,C):- write(M),nl,write(Kd2),nl,loop(Ms,Kd2,C),!.
    
    go(5).
    go(N):- N=<4,writenum(3,'='),write(N),writenum(3,'='),nl,puzzle(N,Kd1,Ms,_),replay(Ms,Kd1),nl, Co is N + 1,go(Co).



