(*open Core*)

(* Initialize the stack to hold type Hashtbl *)

type statRet =
    | Normal
    | Break
    | Continue
    | Return of float;;

    
let prints (s : string) = Printf.printf "%s\n" s;;
let print (s : float) = Printf.printf "%f\n" s;;

type sExpr = 
    | Atom of string
    | List of sExpr list;;

type expr =                         (*The sometype for expressions*)
    | Paren of string*expr*string   (*Parens*)
    | Num of float                  (*Base number*)
    | Var of string                 (*Variable Reference*)
    | Op1 of string*expr            (*Unary Operator*)
    | Op2 of string*expr*expr       (*Binary Operator*)
    | Math of string*expr*string    (*Math operators*)
    | Fct of string * expr list     (*Function*);;

type statement =                                      (*Statement: Call that do stuff lol*)
    | Assign of string*expr                           (*Assignment: Assigns a var to a value*)
    | Print of string*expr                            (*Print*)
    | Return of expr                                  (*Return: Special Case to pull from block*)
    | Expr of expr                                    (*Expresssion to evaluate*)
    | If of expr*statement list * statement list      (*If *)
    | While of expr*statement list                    (*While*)
    | For of statement*expr*statement*statement list  (*For*)
    | FctDef of string * string list * statement list (*Def a function*)
    | Break
    | Continue

type sPair =
    | Nothing  
    | Ret of statRet                    
    | VarPair of string * float                              (*Used to capture variable pairs*)
    | FctPair of string * string list * statement list       (*used to store funtions*);;

let get_pair_val (_pair: sPair): float = match _pair with
    | VarPair(str,flt) -> flt
    | _ -> 0.;;

type block = statement list;;

type env = sPair list;;

type envQueue = env list;;

type progState = 
    | Nothing
    | State of statRet * envQueue;;

let get_pair_var (_s: string) (_pair: sPair): float = match _pair with
    | VarPair(str,flt) -> if (compare str _s = 0) then flt else 0.
    | _ -> 0.;;

let rec search_env (_s: string) (_e: env): float = match _e with
    | [] -> 0.
    | a::tl -> if ((get_pair_var _s a) == (get_pair_val a)) then (get_pair_val a) else search_env _s tl ;; 

let rec search_que (_s: string) (_pS: progState): float = match _pS with 
    | State(state,_q) ->
        match _q with
            | [] -> 0.
            | a::tl -> if ((search_env _s a) = 0.) then (search_que _s (State(Normal,tl))) else (  search_env _s a);;

(* 
let get_pair_fct (_s: string) (_pair: sPair): FctPair = match _pair with
    | FctPair(str,flt) -> if (compare str _s = 0) then flt else 0.
    | _ -> 0.;; *)


let rec search_env_fct (_s: string) (_e: env): sPair = match _e with
    | [] -> Nothing
    | a::tl -> match a with 
        | FctPair(name,param,code)   -> if (compare name _s = 0) then (FctPair(name,param,code)) else (search_env_fct _s tl)
        | _ -> Nothing ;;

let rec search_que_fct (_s: string) (_pS: progState): FctPair = match _pS with 
| State(state,_q) ->
    match _q with
        | [] -> 0.
        | a::tl -> if ((search_env _s a) = 0.) then (search_que _s (State(Normal,tl))) else (  search_env _s a);;

let varEval (_v: string) (_pS: progState): float = search_que _v _q;; 

let evalFct (_v: string) (_code: block) (_pS: progState) = match _pS with 
    | State(state,_q) ->
        ;;

let defFct (_str: string) (_params: string list) (_code: statement list) (_pS: progState): envQueue = 
    [[FctPair(_str, _params, _code)]] @ _q ;;

let rec evalExpr (_e: expr) (_p: progState): float  = 
    match _e with 
        | Num(x) -> x
        | Var(x) -> varEval x _p
        | Paren("(", x, ")") -> evalExpr x _p   
        | Op1(str, x) -> 
                        (match str with
                            | "++" -> (evalExpr x _p) +. 1.
                            | "--" -> (evalExpr x _p) -. 1.
                            | "!" -> if ((evalExpr x _p) != 0.) then 0. else 1.
                            | _ -> 0.0 )
        | Op2(str, x, y) -> 
                        (match str with
                            | "^" -> (evalExpr x _p) ** (evalExpr y _p)
                            | "*" -> (evalExpr x _p) *. (evalExpr y _p)
                            | "/" -> (evalExpr x _p) /. (evalExpr y _p)
                            | "+" -> (evalExpr x _p) +. (evalExpr y _p)
                            | "-" -> (evalExpr x _p) -. (evalExpr y _p)
                            | "&&" -> if((evalExpr x _p) != 0. && (evalExpr y _p) != 0.) then 1. else 0.
                            | "||" -> if((evalExpr x _p) != 0. || (evalExpr y _p) != 0.) then 1. else 0.
                            | "==" -> if((evalExpr x _p) = (evalExpr y _p)) then 1. else 0.
                            | "!=" -> if((evalExpr x _p) = (evalExpr y _p)) then 0. else 1.
                            | ">=" -> if((evalExpr x _p) >= (evalExpr y _p)) then 1. else 0.
                            | "<=" -> if((evalExpr x _p) <= (evalExpr y _p)) then 1. else 0.
                            | ">" -> if((evalExpr x _p) > (evalExpr y _p)) then 1. else 0.
                            | "<" -> if((evalExpr x _p) < (evalExpr y _p)) then 1. else 0.
                            | _ -> 0.0 )
        | Math(str, x, ")") -> 
                        (match str with
                            | "s(" -> (sin (evalExpr x _p))
                            | "c(" -> (cos (evalExpr x _p))
                            | "e(" -> (exp (evalExpr x _p))
                            | "l(" -> (log (evalExpr x _p))
                            | _ -> 0.0 )
        | Fct(str, [x]) -> evalFct str [x] _p
        | _ -> 0.0 (*some kind of error here*);;

(* Test for expression *)
(*let%expect_test "evalNum" = 
    evalExpr (Num 10.0) [] |>
    printf "%F";
    [%expect {| 10. |}]*)

let pop lst = match lst with 
    | [] -> []
    | _::tl -> tl ;;


let defFct (_str: string) (_params: string list) (_code: statement list) (_p): progState = 
    [[FctPair(_str, _params, _code)]] @ _p ; Nothing


let evalAssign (_v: string) (_e: expr) (_p: progState): progState = 
    let e = evalExpr _e _p in match _p with
        | State(state, _q) -> match state with
            | Normal -> State(Normal, [[VarPair(_v, e)]] @ _q)
            | _ -> State(state, _q)
        | _ -> Nothing

let rec evalStatement (s: statement) (p: progState): progState =
    match s with 
        | Assign(_v, _e) -> evalAssign _v _e p
        (*| Return(e) -> Return(Num((evalExpr e p)))*)
        | Expr(e) -> (match p with 
                        | State(state, q) -> (match state with 
                            | Normal -> print (evalExpr e p); State(Normal, q) 
                            | _ -> State(state, q))
                        | _ -> Nothing)
        | Print(str, x)-> (match p with 
                        | State(state, q) -> (match state with 
                            | Normal -> print (evalExpr x p); p
                            | _ -> p)
                        | _ -> Nothing)
        | Break -> (match p with
                        | State(state, q) -> State(Break, q)
                        | _ -> Nothing)
        | Continue -> (match p with
                        | State(state, q) -> State(Continue, q)
                        | _ -> Nothing)
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e p in
                if(cond>0.0) then
                    evalCode codeT p 
                else
                    evalCode codeF p
        | While(e, code) -> (*(match p with
                                | Normal -> evalWhile e code p
                                | Break -> evalWhile Num(-1) [] State(Normal, _q)
                                | Continue -> )*) evalWhile e code p
        (*| For(int, bool, inc, code) ->  let que = evalStatement int p in
                                            evalFor bool inc code que
        (*| FctDef(str, params, code) -> defFct str params code q *)
        (*| _ -> q (*ignore *) (*throw error here *)*)
        and evalFor (_bool: expr) (_inc: statement) (_code: statement list) (_pS: progState): envQueue = 
            let cond = evalExpr _bool _q in
                if(cond>0.) then
                    let q = evalCode _code _q in 
                        let que = evalStatement _inc q in
                            evalFor _bool _inc _code que
                else
                     _q*)
            
        and evalWhile (_e: expr) (_code: statement list) (_p: progState): progState =
        let cond = evalExpr _e _p in
            if(cond>0.) then
                let q = evalCode _code _p in 
                    match q with
                        | State(state, _q) -> match state with
                            | Break -> evalWhile (Num(-1.)) [] (State(Normal, _q))
                            | _ -> evalWhile _e _code _p
            else
                _p

        and eval_states (_pS: progState) (code: block): progState = match _pS with
        | State(state,_q) -> match state with
            | Normal -> (match code with 
                | [] -> let newQ = pop _q in State(state,newQ)   (* pop the local environment *)
                | a::tl -> let ret = evalStatement a _pS in eval_states ret tl;)
            | _ -> _pS

        and evalCode (_code: block) (_pS: progState): progState = match _pS with
        | State(state,_q) ->
            let que = [[]] @ _q in           (* create new environment *)
                eval_states (State(state,que)) _code ;; 

    

    


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

let whileBlockTest1 = [Assign("i", Num(3.)); 
                       While(Op2("!=", Var("i"), Num(4.)), [Print("print", Op2("+", Num(10.), Var("i")));
                                                            Assign("i", (Op1("++", Var("i"))));
                                                            Print("print", Var("i"))])]

let whileBlockTest2 = [Assign(("i"), Op2("-", Num(5.), Num(4.)));
                        While(Op2("<", Var("i"), Num(10.)), [Assign("i", (Op1("++", Var("i"))));
                                                             (*Continue;
                                                             Assign("i", (Op1("--", Var("i"))))*)])]





let testEnv = [[VarPair("x", 1.); VarPair("y", 2.); VarPair("z", 3.)]]

(*let test = evalStatement (Assign("z", Num(5.))) []

let test2 = evalStatement (Assign("z", Num(8.))) []*)

let testBlock = [ Assign("i", Num(1.)); Assign("i", Op1("++", Var("i"))); Print("print", Var("i")); Print("print", Op2("!=", Var("i"), Num(3.))) ]

let main = evalCode whileBlockTest1 (State(Normal, testEnv))