(* FACTORIAL USING KARATSUBA MULTIPLICATION *)
(* VIVEK SINGH IIT D 2019MCS2574*)

exception Invalid_Input_exception of string;



(* removing leading zeroes from ans *)
fun 	rlz([])=[]
|	rlz(x::y)= if x=0 then rlz(y)
 		else x::y;
 		
 		
 		
(* FROM STRING*)
(* Function to convert string to int list *)
fun fs x = x-48;
fun	fromString("")=[]
|	fromString(s)= map fs (map Char.ord(explode s) ); 


			


(* TO STRING *)
(* Function converts INT->INT LIST *)
fun	inttolist(0,listnew)=listnew         
|	inttolist(x,listnew)=inttolist(x div 10, (x mod 10)::listnew);

(* padding 0s logic is written here *)
fun	paddzero(x)= if x=0 then (0::0::0::0::inttolist(x,[]))
		else if x<10 then (0::0::0::inttolist(x,[]))
		else if  x<100 then (0::0::inttolist(x,[]))
		else if  x<1000 then (0::inttolist(x,[]))
		else inttolist(x,[]);
		
(* this takes int list-> int list by padding 0s *)
fun 	tointlist([],newlist)=newlist
|	tointlist(x::y,newlist)=tointlist(y,  newlist@paddzero(x)) ;

(* FUNCTION to convert INT LIST of 0s padded ->STRING *)
fun fa x = x+48;
fun inttocharlist(x)= map Char.chr (map fa(rlz(tointlist(x,[]))));
fun toString(x)= implode (inttocharlist(x));





(*Calculates length of list O(n)*)
fun 	len([])=0
|	len(a::b)= 1+len(b);


(*returns reversed list in listnew  O(n)*)
fun	reverse([],listnew)= listnew		
|	reverse(a::b,listnew)= reverse(b,a::listnew); 

(* compatible length *)
fun	equallen1(x,y,0)= (x,y)
|	equallen1(x,y,count)= equallen1(x,0::y,count-1);
fun	equallen2(x,y,0)= (x,y)
|	equallen2(x,y,count)= equallen2(0::x,y,count-1);
fun compatible(X,Y)= let 
			val xl=len(X)
			val yl=len(Y)
			in
			if xl>yl then equallen1(X,Y,xl-yl)
			else if xl<yl then equallen2(X,Y,yl-xl)
			else (X,Y)
			end;
			
		     
		     
		     
		     
		     
(* Function to convert Int list to base of 10^4 int List (int list->int list)  *)
fun 	groupfour([],_,0,listf)= listf                  (*this is case if numb=0 then dont insert in listf *)
|	groupfour([],_,numb,listf)= numb::listf		(*this is case if elements are not multiple of 4 then to insert into listf *) 
|	groupfour(lists as a::b ,count,numb,listf)= 	(* 4 cases to convert into 10^4 base numb *)	
	if count=3 then groupfour(b,2,numb+a,listf)
	else if count=2 then groupfour(b,1,numb+a*10,listf)
	else if count=1 then groupfour(b,0,numb+a*100,listf)
	else groupfour(b,3,0,(numb+a*1000)::listf); (* this is case is elements are multiple of 4 then insert in listf *)
	
fun stringtointlist(s)=groupfour(reverse(fromString(s),[]),3,0,[]);




		     
(* Addition of base 10^4 lists *)
(*  no need to make lists compatible as addl takes care of it *) 
fun	addl_logic([],[],1,newlist)=1::newlist
|	addl_logic([],[],0,newlist)=newlist
|	addl_logic(x,[],_,newlist)=newlist
|	addl_logic([],y,_,newlist)=newlist
|	addl_logic(x1::x2,y1::y2,C,newlist)= if C=1 andalso (x1+y1+1)>=10000 then addl_logic(x2,y2,1,(x1+y1+1-10000)::newlist)
					else if C=1 andalso (x1+y1+1)<10000  then addl_logic(x2,y2,0,(x1+y1+1)::newlist)
					else if C=0 andalso (x1+y1)>=10000 then addl_logic(x2,y2,1,(x1+y1-10000)::newlist)
					else addl_logic(x2,y2,0,(x1+y1)::newlist);

					
					
fun	addl(x,y)=let
		val (X,Y)=compatible(x,y)
		in 
		addl_logic(reverse(X,[]),reverse(Y,[]),0,[])
		end;





(* Subtraction of base 10^4 lists *)
(* we will make sure that passed lists for subtraction are already compatible so no need to aplly compatible here in subl *)
fun	largeroftwo([],[])= 1
|	largeroftwo(x1,[])=1
|	largeroftwo([],y1)= ~1
|	largeroftwo(x1::x2,y1::y2)= if x1>y1  then 1
				else if x1<y1 then ~1
				else largeroftwo(x2,y2);
				
fun	subl_logic([],[],_,newlist)=newlist
|	subl_logic([],y,_,newlist)=newlist (* for match nonexhaustive Warning *)
|	subl_logic(x,[],_,newlist)=newlist (* for match nonexhaustive Warning *)
|	subl_logic(x1::x2,y1::y2,B,newlist)= if B=1 andalso (x1-y1-1)<0 then subl_logic(x2,y2,1,(10000+x1-y1-1)::newlist)
					else if B=1 andalso (x1-y1-1)>=0 then subl_logic(x2,y2,0,(x1-y1-1)::newlist)
					else if B=0 andalso (x1-y1)<0 then subl_logic(x2,y2,1,(10000+x1-y1)::newlist)
					else subl_logic(x2,y2,0,(x1-y1)::newlist);
					
					
fun subl(a,b)=  let
                val(x,y)=compatible(a,b)
                in
		if largeroftwo(x,y)=1 then subl_logic(reverse(x,[]),reverse(y,[]),0,[])
		else subl_logic(reverse(y,[]),reverse(x,[]),0,[])
		end;
		
		
		
		
		
		
		
(* Appending zeroes in List to simulate Base ^ length of string *)
 fun	appendzero_logic(x,0)=x
 |	appendzero_logic(x,count)=appendzero_logic(0::x,count-1);
 
 fun	appendzero(x,count)=let 
 			val X=reverse(x,[])
 			in 
 			reverse(appendzero_logic(X,count),[])
 			end;
 			
 			
 			
 			
 			
 (* Splitting function that divides a list into two half part *)
 fun	split([],_,newlist)=(newlist,[]) (* for match nonexhaustive Warning *) 
 |	split(X, 0,newlist)=(reverse( newlist,[]),X)
 |	split(X as x1::x2,halflen ,newlist)=split(x2,halflen-1,x1::newlist);
 
 
 
 
 
 
 (* Writing the main Core for multiplication using Karatsuba algo *)
 (* must make lists compatible before calling subl *)
 (* no need to make list compatible for addl as it takes care of it in its algo *)
 
fun	karatsuba_algo([a],[b])=[(a*b) div 10000,(a*b) mod 10000] 
  |	karatsuba_algo(x,y)= let			     
   			     val(x1,x0)=split(x,len(x) div 2,[])
   			     val(y1,y0)=split(y,len(y) div 2,[])
   			     val(X0,X1)=compatible(x0,x1)
   			     val(Y1,Y0)=compatible(y1,y0)
   			     val x1m=rlz(x1)
   			     val x0m=rlz(x0)
   			     val y1m=rlz(y1)
   			     val y0m=rlz(y0)
   			     val sx0x1=subl(X0,X1)
   			     val sy1y0=subl(Y1,Y0)
   			     val sx0x1m=rlz(sx0x1)
   			     val sy1y0m=rlz(sy1y0)
   			     val z2=if x1m=[] orelse y1m=[] then [0]
   			            else karatsuba_algo(x1,y1)
   			            
   			     val z1=if sx0x1m=[] orelse sy1y0m=[] then [0]
   			     	    else karatsuba_algo(sx0x1,sy1y0)
   			     	    
  			     val z0=if x0m=[] orelse y0m=[] then [0]
   			            else karatsuba_algo(x0,y0)
   			            
  			     val sign1=largeroftwo(X0,X1)
  			     val sign2=largeroftwo(Y1,Y0)
  			     val z11=addl(z2,z0)
  			     val pow=(len(x)+1) div 2
  			     val z22=appendzero(z2,2*pow)
  			     in
  			     if sign1*(sign2)=1 then addl(addl(appendzero(addl(z11,z1),pow),z0),z22)
  			     else addl(addl(appendzero(subl(z11,z1),pow),z0),z22)
 			     end;
 
 fun karatsuba x y=let
 		val (X,Y)=compatible(x,y)
 		in rlz(karatsuba_algo(X,Y))
 		end;
 
 		

 
 
 (* factorial program using karatsuba *)
 fun 	check_exception([])=false
 |	check_exception(x::y)= if x<0 orelse x>9 then true
 			else check_exception(y);
 
 fun dec(x)=  let
 		val (a,b)=compatible(x,[1])
 		in rlz(subl(a,b))
 		end;
 
 fun factorial_logic([0])=[1]
 |	factorial_logic([1])=[1]
 |	factorial_logic(x)=karatsuba x (factorial_logic(dec(x)));
 
 
 fun 	factorial("0")="1"
 |	factorial(s)= let
                      val st= Time.now()
 		      val output=if check_exception(fromString(s)) then raise Invalid_Input_exception(s)
                                 else toString(factorial_logic(stringtointlist(s)))
                      val finatime=(print(Time.toString(Time.-(Time.now(),st)));8)
                      in
                      output
                      end
                      handle Invalid_Input_exception(s)=> (print("Invalid_Input_exception "^s^"\n");"");
                     
                      
 
 
 
 			     
 			     
 			
 
 
 
 
 
 
 
 




















