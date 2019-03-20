(*open Core*)

let globalScope = Stack.create ();;
let ht = Hashtbl.create 123456;;
Hashtbl.add ht "x" 0.;;

(* Initialize the stack to hold type Hashtbl *)
Stack.push ht globalScope;;
Stack.pop globalScope;;

type statRet =
    | Normal
    | Break
    | Continue
    | Return of float

let prints (s : string) = Printf.printf "%s\n" s;;
let print (s : float) = Printf.printf "%f\n" s;;

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr =                         (*The sometype for expressions*)
    | Paren of string*expr*string   (*Parens*)
    | Num of float                  (*Base number*)
    | Var of string                 (*Variable Reference*)
    | Op1 of string*expr            (*Unary Operator*)
    | Op2 of string*expr*expr       (*Binary Operator*)
    | Math of string*expr*string    (*Math operators*)
    | Fct of string * expr list     (*Function*)

type sPair =
    | Nothing                        
    | VarPair of string * float                              (*Used to capture variable pairs*)
    (*| FctPair of string * string list * statement list       (*used to store funtions*)*)

let get_pair_val (_pair: sPair): float = match _pair with
    | VarPair(str,flt) -> flt
    | _ -> 0.;;

type statement =                                      (*Statement: Call that do stuff lol*)
    | Assign of string*expr                           (*Assignment: Assigns a var to a value*)
    | Return of expr                                  (*Return: Special Case to pull from block*)
    | Expr of expr                                    (*Expresssion to evaluate*)
    | If of expr*statement list * statement list      (*If *)
    | While of expr*statement list                    (*While*)
    | For of statement*expr*statement*statement list  (*For*)
    | FctDef of string * string list * statement list (*Def a function*)

type block = statement list 

type env = sPair list (* complete *)

type envQueue = env list

let get_pair_var (_s: string) (_pair: sPair): float = match _pair with
    | VarPair(str,flt) -> if (compare str _s = 0) then flt else 0.
    | _ -> 0.;;

let rec search_env (_s: string) (_e: env): float = match _e with
    | [] -> 0.
    | a::tl -> if ((get_pair_var _s a) == (get_pair_val a)) then (get_pair_val a) else search_env _s tl ;; 

let rec search_que (_s: string) (_q: envQueue): float = match _q with
    | [] -> 0.
    | a::tl -> if ((search_env _s a) = 0.) then ( search_que _s tl) else (  search_env _s a);;

let varEval (_v: string) (_q: envQueue): float = search_que _v _q 

let evalFct (_v: string) (_e: expr list) (_q: envQueue) = 0.0

let rec evalExpr (_e: expr) (_q: envQueue): float  = 
    match _e with 
        | Num(x) -> x
        | Var(x) -> varEval x _q
        | Paren("(", x, ")") -> evalExpr x _q   
        | Op1(str, x) -> 
                        (match str with
                            | "++" -> (evalExpr x _q) +. 1.
                            | "--" -> (evalExpr x _q) -. 1.
                            | _ -> 0.0 )
        | Op2(str, x, y) -> 
                        (match str with
                            | "^" -> (evalExpr x _q) ** (evalExpr y _q)
                            | "*" -> (evalExpr x _q) *. (evalExpr y _q)
                            | "/" -> (evalExpr x _q) /. (evalExpr y _q)
                            | "+" -> (evalExpr x _q) +. (evalExpr y _q)
                            | "-" -> (evalExpr x _q) -. (evalExpr y _q)
                            | "==" -> if((evalExpr x _q) == (evalExpr y _q)) then 1. else 0.
                            | ">=" -> if((evalExpr x _q) >= (evalExpr y _q)) then 1. else 0.
                            | "<=" -> if((evalExpr x _q) <= (evalExpr y _q)) then 1. else 0.
                            | ">" -> if((evalExpr x _q) > (evalExpr y _q)) then 1. else 0.
                            | "<" -> if((evalExpr x _q) < (evalExpr y _q)) then 1. else 0.
                            | _ -> 0.0 )
        | Math(str, x, ")") -> 
                        (match str with
                            | "s(" -> (sin (evalExpr x _q))
                            | "c(" -> (cos (evalExpr x _q))
                            | "e(" -> (exp (evalExpr x _q))
                            | "l(" -> (log (evalExpr x _q))
                            | _ -> 0.0 )
        | Fct(str, [x]) -> evalFct str [x] _q
        | _ -> 0.0 (*some kind of error here*)

(* Test for expression *)
(*let%expect_test "evalNum" = 
    evalExpr (Num 10.0) [] |>
    printf "%F";
    [%expect {| 10. |}]*)

let evalCode (_code: block) (_q: envQueue): unit = 
    (* crate new environment *)
    let que = [[]] @ _q in ()
    (* user fold_left  *)
    (* pop the local environment *)
    (*let scope = Stack.top globalScope in
        let eval = evalStatement Assign("v" (Num(5))) scope*)
    (*print_endline "does this do something?"*)


let defFct (_str: string) (_params: string list) (_code: statement list) (_q: envQueue) = 0.0

let evalFor (_int: statement) (_bool: expr) (_inc: statement) (_code: statement list) (_q: envQueue) = 
    (*let cond = evalExpr _bool _q in*)
        0.0

let rec evalStatement (s: statement) (q: envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> (* eval e and store in v *) q
        | Return(e) -> q (*evalExpr e q *) (*idk*)
        | Expr(e) -> (print (evalExpr e q)); q 
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalCode codeT q 
                else
                    evalCode codeF q
            ;q (*i think something goes here *)
        | While(e, code) ->    (*use recursive calls instead*)
            let cond = evalExpr e q in 
                while(cond>0.0) do
                    evalCode code q 
                done
            ;q (*i think something goes here *)
        | For(int, bool, inc, code) -> evalFor int bool inc code q
            ;q                                      (*these two causes the warnings idk why*)
        | FctDef(str, params, code) -> defFct str params code q 
            ;q
        (*| _ -> q (*ignore *)*) (*throw error here *)


(* 
    v = 10; 
    v // display v
 *)

(* let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
];

(*let%expect_test "p1" =
    evalCode p1 []; 
    [%expect {| 1. |}]


    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]; *)

(*let%expect_test "p1" =
    evalCode p2 []; 
    [%expect {| 3628800. |}]

  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
(* let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ];

(*let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]*)*)



let testEnv = [[VarPair("x", 1.); VarPair("y", 2.); VarPair("z", 3.)]]


let main = evalStatement (Expr(Var("z"))) testEnv