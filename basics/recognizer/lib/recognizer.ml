let rec lang1 l = match l with 
  | [x] when x == '1' || x=='0' -> true
  | x :: l when x == '1' || x == '0' -> true && lang1 l 
  |_ -> false ;;

let rec lang2_rec l =  match l with 
  |[] -> true 
  | [x] when x == '0' || x == '1'  -> true 
  | '1' :: l -> true && lang2_rec l 
  |_ -> false;;

let lang2 l = lang2_rec (List.rev(l));;

let rec lang3_rec l i = match l with
|['0'] -> true
| '0' :: l when i = 0 -> true && lang3_rec l (i+1)
| x :: l when x == '0' || x == '1' -> true && lang3_rec l (i+1)
|_ -> false;;


let rec lang3_rec l i = match l with
  |['0'] when i >= 1 -> true
  | '0' :: l when i = 0 -> true && lang3_rec l (i+1)
  | x :: l when x == '0' || x == '1' -> true && lang3_rec l (i+1)
  |_ -> false;;

let lang3 l = lang3_rec l 0 ;;


let rec lang4_rec l i = match l with
  |[] -> true
  | '0' :: l when i == 0 -> true && lang4_rec l i
  | '1' :: '0' :: l -> true && lang4_rec l (i+1)
  |_ -> false ;;

let lang4 l = lang4_rec l 0 ;;


let rec lang5_rec l i = match l with 
  |[] when i > 0 -> true 
  |'1' :: '1' :: l -> true && lang5_rec l (i+1)
  |'0' :: '0' :: l -> true && lang5_rec l (i+1)
  |_ -> false;;
    
let lang5 l = lang5_rec l 0 ;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
