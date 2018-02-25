set CM.Control.verbose false;


 (*Συνάρτηση (parse) που διαβάζει από το map.txt και περιέχει συνάρτηση που τοποθετεί τα στοιχεία μου σε λίστα από τούπλες*)
      fun parse file =
          let
          (* Συνάρτηση που διαβάζει το input απο ενα συγκεκριμένο αρχείο *)
              fun read_int input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC)  input)
              fun read_real input = Real.fromString (Option.valOf (TextIO.inputLine input) )
              (* Aνοιξε το αρχείο εισόδου *)
              val inStream = TextIO.openIn file
              val N = read_int inStream

          (* Συνάρτηση που περνάει τα στοιχεία που διάβασα από την λίστα σε μια λίστα από τούπλες της [(B,H,W,L)] *)
              fun parseline 0 acc = ( acc , read_int inStream )
                | parseline i acc = 
                  let 
                        val X1 = read_int inStream
                        val Y1 = read_int inStream
                        val X2 = read_int inStream
                        val Y2 = read_int inStream
                  in
                        parseline (i-1) ((X1, Y1, X2, Y2)::acc) (*Προστίθεται στην αρχή κάθε νέο στοιχείο*)
                  end                                           (*Αυτό που προκύπτει είναι μια αντεστραμένη λίστα*)
             
               
              
          in
              parseline N []  (*με αυτήν την κλήση φτιάχνω την λίστα μου*)

                end
          
         
          (*H λίστα έχει μήκος N, άρα περιέχει N τούπλες της μορφής ((B,H,W,L)*)
          
(*Η συνάρτηση Merge_sort1 που ακολουθεί ταξινομεί τη λίστα από τούπλες σε αύξουσα σειρά
με βάση την τιμή του πρώτου στοιχείου (base) της κάθε τούπλας*) 

      fun Merge_sort1 nil = nil
        | Merge_sort1 [e] = [e]
        | Merge_sort1 element_list  =
              let
                fun halve nil = (nil, nil)
                  | halve [a] = ([a], nil)
                  | halve (a :: b :: rest) = 
                  let
                      val (x, y) = halve rest
                    in
                      (a :: x, b :: y)
                    end  

                fun merge1 (nil, ys) = ys
                  | merge1 (xs, nil) = xs
                  | merge1 ((X11, Y11, X12) :: xs, (X21,Y21,X22)::ys) =
                    if ( X11 <  X21) then (X11, Y11, X12) :: merge1(xs, (X21,Y21,X22) :: ys)
                    else (X21,Y21,X22)::merge1 ((X11, Y11, X12) :: xs, ys);
                
                val (x, y) = halve element_list 
              in
                merge1 (Merge_sort1 x , Merge_sort1 y )
              end


 
(*Η συνάρτηση Merge_sort2 που ακολουθεί ταξινομεί τη λίστα από τούπλες σε αύξουσα σειρά
με βάση την τιμή του δεύτερου στοιχείου (height) της κάθε τούπλας*) 

      fun Merge_sort2 nil = nil
        | Merge_sort2 [e] = [e]
        | Merge_sort2 element_list  =
              let
                fun halve nil = (nil, nil)
                  | halve [a] = ([a], nil)
                  | halve (a :: b :: rest) = 
                  let
                      val (x, y) = halve rest
                    in
                      (a :: x, b :: y)
                    end  

                fun merge2 (nil, ys) = ys
                  | merge2 (xs, nil) = xs
                  | merge2 ((X11, Y11, X12) :: xs, (X21,Y21,X22)::ys) =
                    if ( Y11 <  Y21) then (X11, Y11, X12) :: merge2 (xs, (X21,Y21,X22) :: ys)
                    else (X21,Y21,X22)::merge2 ((X11, Y11, X12) :: xs, ys);
                
                val (x, y) = halve element_list 
              in
                merge2 (Merge_sort2 x , Merge_sort2 y )
              end

(*θελω να φτιαξω μια συναρτηση που θα παιρνει μια ΤΑΞΙΝΟΜΗΜΕΝΗ λιστα με βαση το b απο τουπλες 4 στοιχειων (b,h,w,l)
και θα επιστρεφει τη λιστα με τούπλες (b,0,w*l,)  ,όπου w*l είναι η βάση της δεξαμενής και ο ρυθμός αύξησης
της χωρητικότητας της δεξαμενής ανά επίπεδο *)
fun change1 (a1,b1,c1,d1) = (a1,0,c1*d1);
fun change2 (a1,b1,c1,d1 ) = (a1+b1,1,c1*d1);
     
(*με τη σχέση map change1 (ή change2 αντίστοιχα) list έχω την εφαρμογή της change1/2 σε κάθε στοιχείο της λίστας list*)

    
        
fun solh (( ogkos_old , tempo_old , height_old ) , ( a , 0 , c ) ) = ( ogkos_old - ( tempo_old ) * ( a - height_old ) , tempo_old + c , a )
    |solh ( ( ogkos_old , tempo_old , height_old ) , ( a , 1 , c ) ) = ( ogkos_old - ( tempo_old ) * ( a - height_old ) , tempo_old - c , a ) ;
      
fun solh2 (( ogkos_old , tempo_old , height_old ) , ( a , 0 , c ) ) = ( ogkos_old - ( tempo_old ) * ( a - height_old ) , tempo_old , a )
    |solh2 ( ( ogkos_old , tempo_old , height_old ) , ( a , 1 , c ) ) = ( ogkos_old - ( tempo_old ) * ( a - height_old ) , tempo_old , a ) ;
      

(*apo katw to d -e -f einai to 78,0,0*)

fun please ( (d,e,f) , [(a,b,c)] ) = 
if (d >  ( e ) * ( a - f )) then solh ((d,e,f),(a,b,c)) 
else  solh2 ((d,e,f),(a,b,c))
|
    please ( (ds,es,fs) , ( anos :: xs ) ) =  
    if (ds > ( es ) * ( #1(anos) - fs )) then  please (  solh (   ( ds , es , fs ) , anos ) , xs )
    else  solh2 (   ( ds , es , fs ) , anos ) 
    
fun final (a,b,c) = 
    if (a <= 0) then real (c) + ( real(a) / real (b))
    else ~1.0
    
    
 
fun solution list_a volume = 
    final ( please ( volume , Merge_sort1 ( ( ( Merge_sort1 ( map change1  list_a  )  ) @ ( Merge_sort2 ( map  change2  list_a  ) ) ) ) ) ) ;

fun tel ( a , b ) = solution (a) (b,0,0)
fun deksamenes file =

tel (parse file)

(*  let

     val Lista = (parse file) (*εδώ καλώ την συνάρτηση για να διαβάσει το αρχείο,αυτό που επιστρέφει η συνάρτηση είναι μια λίστα από τούπλες*)
   in 
      solution Lista (78,0,0) 
   end 
   
*)

