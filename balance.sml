fun to3b (n) =
let
  fun to3'b 0   =  [] 
    | to3'b num = num mod 3 :: to3'b (num div 3)
in
  rev (to3'b n)
end

fun ch2 0 = []
| ch2 (N) = N :: ch2 (N-1) 

fun compare ( [] : int list , [] : int list , b : int list) = b :  int list 
| compare ( listaA : int list ,listaB : int list, listaC : int list ) = if ( hd (listaA) = 1 ) then  compare(tl (listaA) , tl (listaB), length ( listaB ) :: listaC) 
    else compare(tl (listaA) , tl (listaB),listaC)

fun balance0 (  lista : int list  ) =
    ( (rev ( compare ( lista , ch2 ( length ( lista )) , []) ) ) ) : int list
    
fun tupleTwoElements ( a , b , c ) = ( b , c )
 
fun incFirst [] : int list = [] : int list
| incFirst ( lista : int list ) = ( ( 1 + hd ( lista ) )  :: tl ( lista ) ) : int list

fun allaksetatriaria ( []  : int list, a : int list ) = ( [] : int list , rev ( a ) : int list ) 
| allaksetatriaria ( lista : int list , listb : int list ) = 
if (hd ( lista ) = 3 ) then allaksetatriaria ( incFirst( tl (  lista ) ) , 0 :: listb )  
    else  ( allaksetatriaria ( tl lista , hd ( lista )  :: listb) )

fun second ( a : int list , b : int list ) = b : int list

fun convert2 ( [] : int list , a : int list , b : int list ) = ( [] : int list , rev ( a ) : int list, rev ( b ) : int list ) 
 | convert2 ( listaA : int list, listaB : int list , listaC : int list) = 
    if ( hd ( listaA ) = 2) then convert2 ( incFirst( tl ( listaA ) ) , 0 :: ( listaB ) , 1 :: listaC )
    else ( convert2 (tl ( listaA ) , hd( listaA ) :: (listaB ) , 0 :: listaC ) )

fun f ( lista ) = rev (to3b lista) @ [0]

fun soler ( a : int list , b : int list)  = ( a : int list , allaksetatriaria ( b , [] )   )

fun solution x = soler (  tupleTwoElements ( convert2 ( to3b ( x ) , [] , [] ) )  ) 

fun geia ( x : int list , y : int list ) = ( compare ( rev ( x ) , [] , [] ) , compare ( rev ( y ) , [] , [] ) )

fun doit (x) = convert2 ( f ( x ) , [] , [] ) 

fun ksana ( a  : int list , b : int list , c : int list ) = ( c , b )

fun pame ( listaA , listaB , listaC ) = (  balance0 (rev ( listaC) )   , balance0 ( rev  ( listaB ) ) ) 

fun doit2 ( [] : int list , b : int list , c : int list ) =  ( [] : int list ,second ( allaksetatriaria ( b , [] ) ) : int list , c : int list)  

fun teliko x = doit2 ( doit ( x ) )

fun sol x = pame ( teliko x ) 

fun solution ( N : int , (listaA : int list , listaB : int list ) ) = if (hd (listaB) > N ) then ([],[])
else (rev( listaA ) , rev ( listaB ) )

fun balance x y = solution ( x , sol ( y ) )
