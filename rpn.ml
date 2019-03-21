(* Sources:
    1. https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034
    2. https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
    3. https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stack.html
    4. http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1045&context=mathmidexppap
*)

#load "str.cma";;

(* Create a stack to evaluate expressions accordingly *)
let num_stack = Stack.create ();;

(* Initialize the stack to hold type float *)
Stack.push 4.2069 num_stack;;
Stack.pop num_stack;;

(* Functions to eval expressions *)
let add (a : float) (b : float) = a +. b;;
let sub (a : float) (b : float) = a -. b;;
let mul (a : float) (b : float) = a *. b;;
let div (a : float) (b : float) = a /. b;;
let pow (a : float) (b : float) = a ** b;;

(* Functions to make printing not suck *)
let prints (s : string) = Printf.printf "%s\n" s;;
let print (s : float) = Printf.printf "%f\n" s;;

(* Function that evaluates the stack based on character read in *)
let eval (op : string) = 
  try 
    let right = Stack.pop num_stack in
      let left = Stack.pop num_stack in 
        match op with 
          | "+" -> add left right
          | "-" -> sub left right
          | "*" -> mul left right
          | "/" -> div left right
          | "^" -> pow left right
          | _ -> failwith "Invalid operation!"
  with Stack.Empty -> failwith "Invalid use of operator!";;

(* This function processes each line of text, parsing the numbers/operators into the correct stack. *)
let process str = 
  let split = Str.split (Str.regexp " +") in
    let tokens = split str in 
      let push token =
        if Str.string_match (Str.regexp "[-]?[0-9]*.?[0-9]+") token 0 then let flt = float_of_string token in Stack.push flt num_stack
        else if Str.string_match (Str.regexp "\\+\\|-\\|\\*\\|\\/\\|\\^") token 0 then let result = eval token in Stack.push result num_stack
        else failwith (Printf.sprintf "Error for token: %s, not a valid number or operator!" token)
      in List.iter push tokens;;

(* This function based from source 1 opens the test file and calls the process function on each line. *)
let read_lines file process =
  let in_ch = open_in file in
    let rec read_line (line_num : int) =
      let line = try input_line in_ch with End_of_file -> exit 0
      in (* process line in this block, then read the next line *)
        process (line);
        try 
          print (Stack.pop num_stack);
          if not (Stack.is_empty num_stack) then failwith (Printf.sprintf "Error at line %i for token: %f, too many numbers!" line_num (Stack.top num_stack));
          read_line (line_num+1);
        with Stack.Empty -> failwith (Printf.sprintf "Error at line %i, invalid use of operator!" line_num);
in read_line (1);;

(* Test execution. Only can run one text file at a time. *)
read_lines "test.txt" process;;

(* The rest of these tests should result in failure *)
(* read_lines "fail_test1.txt" process;;
read_lines "fail_test2.txt" process;;
read_lines "fail_test3.txt" process;; *)