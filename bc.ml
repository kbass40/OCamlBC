(*open Core*)

type statRet =
    | Normal
    | Break
    | Continue
    | Return of float;;

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
    | Debug of string                                 (*Debug tool to save your eyesight and sanity*)
    | Return of expr                                  (*Return: Special Case to pull from block*)
    | Expr of expr                                    (*Expresssion to evaluate*)
    | If of expr*statement list * statement list      (*If *)
    | While of expr*statement list                    (*While*)
    | For of statement*expr*statement*statement list  (*For*)
    | FctDef of string * string list * statement list (*Def a function*)
    | Break
    | Continue;;

type sPair =
    | Nothing  
    | Ret of statRet                    
    | VarPair of string * float                               (*Used to capture variable pairs*)
    | FctPair of string * string list * statement list        (*used to store funtions*);;

let get_pair_val (_pair: sPair): float = match _pair with
    | VarPair(str,flt) -> flt
    | _ -> 0.;;

type block = statement list;;

type env = sPair list;;

type envQueue = env list;;

type progState = 
    | Nothing
    | State of statRet * envQueue;;


(* Auxialliary Functions *)
let prints (s : string) = Printf.printf "%s\n" s;;


let print (s : float) = Printf.printf "%f\n" s;;


let pop lst = match lst with 
    | [] -> []
    | _::tl -> tl ;;


let pip_str (lst: string list): string = match lst with 
    | [] -> ""
    | a::_ -> a ;;


let pip_expr (lst: expr list): expr = match lst with 
    | [] -> Num(0.)
    | a::_ -> a ;;    


let get_pair_var (_s: string) (_pair: sPair): float = match _pair with
    | VarPair(str,flt) -> if (compare str _s = 0) then flt else 0.
    | _ -> 0.;;

let rec search_env (_s: string) (_e: env): float = match _e with
    | [] -> 0.
    | a::tl -> if ((get_pair_var _s a) == (get_pair_val a)) then (get_pair_val a) else search_env _s tl ;; 

let rec search_que (_s: string) (_pS: progState): float = match _pS with 
    | State(state,_q) ->
        (match _q with
            | [] -> 0.
            | a::tl -> if ((search_env _s a) = 0.) then (search_que _s (State(Normal,tl))) else (  search_env _s a))
    | _ -> 0.;;


let rec search_env_fct (_s: string) (_e: env): sPair = (match _e with
    | [] -> Nothing
    | a::tl -> (match a with 
        | FctPair(name,param,code)   -> if (compare name _s = 0) then (FctPair(name,param,code)) else (search_env_fct _s tl)
        | _ -> Nothing) );;


let rec search_que_fct (_s: string) (_q: envQueue): sPair = match _q with 
    | [] -> Nothing
    | _e::tl -> let pair = (search_env_fct _s _e) in match pair with 
        | FctPair(s,p,c) -> FctPair(s,p,c)
        | _ -> search_que_fct _s (pop _q);;
                                                            

let varEval (_v: string) (_p: progState): float = search_que _v _p;; 


let toNormal (p: progState): progState = 
    (match p with
        | State(s, q) -> State(Normal, q)
        | _ -> Nothing)

let defFct (_str: string) (_params: string list) (_code: statement list) (_p: progState): progState = 
    match _p with
        | State(s, q) -> match s with
            | Normal -> (match q with 
                            | a::tl -> State(Normal, ([[FctPair(_str, _params, _code)] @ a] @ (pop q))))
                            | _ -> _p
            | _ -> State(s, q)
        | _ -> Nothing

let rec evalStatement (s: statement) (p: progState): progState = 
    match s with 
        | Break -> (match p with
                        | State(state, q) -> State(Break, q)
                        | _ -> Nothing)
        | Continue -> (match p with
                        | State(state, q) -> State(Continue, q)
                        | _ -> Nothing)
        | Assign(_v, _e) -> evalAssign _v _e p
        | Return(e) -> (match p with
                        | State(state, q) ->State((Return((evalExpr e p))), q)
                        | _ -> p)
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
        | Debug(s) -> prints s; p
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e p in
                if(cond>0.0) then
                    evalCode codeT p 
                else
                    evalCode codeF p
        | While(e, code) -> evalWhile e code p
        | For(int, bool, inc, code) ->  let que = evalStatement int p in
                                            evalFor bool inc code que
        | FctDef(str, params, code) -> defFct str params code p
        and evalFor (_bool: expr) (_inc: statement) (_code: statement list) (_pS: progState): progState = 
            let cond = evalExpr _bool _pS in
                if(cond>0.) then
                    let pS = evalCode _code _pS in 
                        (match pS with
                            | State(state, _q) -> (match state with
                                | Break -> (State(Normal, _q))
                                | Continue -> let que = evalStatement _inc _pS in 
                                        evalFor _bool _inc _code (toNormal que)
                                | _ ->  let que = evalStatement _inc pS in
                                        evalFor _bool _inc _code que))
                else
                     _pS
            
        and evalWhile (_e: expr) (_code: statement list) (_p: progState): progState = 
        let cond = evalExpr _e _p in
            if(cond>0.) then
                let q = evalCode _code _p in 
                    (match q with
                        | State(state, _q) -> match state with
                            | Break -> (State(Normal, _q))
                            | Continue -> evalWhile _e _code (State(Normal, _q))
                            | _ -> evalWhile _e _code q)
            else
                _p

        and eval_states (_pS: progState) (code: block): progState = (match _pS with
        | State(state,_q) -> (match state with
            | Normal -> (match code with 
                | [] -> State(state,_q)
                | a::tl -> let ret = evalStatement a _pS in eval_states ret tl;)
            | Return(flt) -> State((Return(flt), (pop _q))) (* pop the local environment *)
            | _ -> _pS)
        | _ -> _pS)

        and evalCode (_code: block) (_pS: progState): progState = match _pS with
        | State(state,_q) -> eval_states (State(state,_q)) _code 
        | _ -> _pS

        and assignParams (_params: string list) (_exprs: expr list) (_e: env): env = match _params with
        | [] -> _e
        | _ ->
            let p = pip_str _params in 
                let v = pip_expr _exprs in
                    let newEnv = assignParam p v _e in
                        assignParams (pop _params) (pop _exprs) newEnv

        and assignParam (_v: string) (_expr: expr) (_e: env): env = [VarPair(_v,(evalExpr _expr (State( Normal, [_e]))))] @ _e

        and evalFct (_v: string) (_exprs: expr list) (_pS: progState): float = (match _pS with
        | State(s, _q) -> (match _q with
            | [] -> 0.
            | _e::tl -> let pair = search_que_fct _v _q in match pair with 
                | FctPair(name,param,code) -> 
                    let blockEnv = assignParams param _exprs _e in
                        let newQ = [blockEnv] @ _q in
                            let localPS = State(s,newQ) in
                                let finishedPS = evalCode code localPS in
                                    let State(ret,que) = finishedPS in (match ret with
                                        | Return(retVal) -> retVal
                                        | _ -> 0.)))

        and evalAssign (_v: string) (_e: expr) (_p: progState): progState = 
            let e = evalExpr _e _p in match _p with
                | State(state, _q) -> match state with
                    | Normal -> (match _q with 
                                    | a::tl -> State(Normal, ([[VarPair(_v, e)] @ a] @ (pop _q))))
                                    | _ -> _p
                    | _ -> State(state, _q)
                
                                        
        and evalExpr (_e: expr) (_p: progState): float  = 
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



let runCode (_block: block)= 
    evalCode _block (State(Normal, [[]]))


let mathBlock = [Debug("Test 10: Basic Math Test");Assign("v",(Op2("+", Var("v"), Num(5.))));
                Expr(Math("s(", Num(1.), ")"));
                Assign("v", Op1("++", Var("v")));
                Assign("x", Var("v"));
                Print("print", Var("x"));
                Expr(Op2("==", Var("x"), Var("v")));
                Debug("------Expect------\n\t0.841471\n\t6\n\t1\t")]

let boolBlock = [Debug("Test 9: Basic Boolean Test");Assign("x", Num(5.));
                Expr(Op2("==", Var("x"), Num(2.)));
                Expr(Op1("!", Var("x")));
                Expr(Op2(">=", Var("x"), Num(1.)));
                Expr(Op2("<", Var("x"), Num(10.)));
                Debug("------Expect------\n\t0\n\t0\n\t1\n\t1")]

let functionBlock = [Debug("Test 8: Basic Function Return Test");Assign("y", Num(69.));
                    FctDef("foo", ["x"], [Return(Var("y"))]);
                    Assign("x", (Fct("foo", [Num(5.)])));
                    Print("print", Var("x"));
                    Debug("------Expect------\n\t69")]

let factorialBlock = [Debug("Test 7: Recursive Factorial Function Test");FctDef("factorial", ["n"], [If((Op2("==", Var("n"), Num(1.)), [(Return(Num(1.)))], 
                                                                                    [Return(Op2("*", Var("n"), Fct("factorial", [Op1("--", Var("n"))])))]))]);
                       Print("print", Fct("factorial", [Num(5.)]));
                       Debug("------Expect------\n\t120")]

let functionBlock2 = [Debug("Test 11: Add Function Test");Assign("x", Num(5.));
                      FctDef("foo", ["x"; "y"], [Assign("x", Op2("+", Var("x"), Var("y")));
                                                 Return(Var("x"))]);
                      Assign("x", Fct("foo", [Num(20.)]));
                      Print("print", Var("x"));
                      Debug("------Expect------\n\t20")]

let whileBlockTest1 = [Debug("Test 4: Basic While-Loop Test");Assign("i", Num(3.)); 
                       While(Op2("!=", Var("i"), Num(12.)), [Print("print", Op2("+", Num(10.), Var("i")));
                                                            Assign("i", (Op1("++", Var("i"))))]);Debug("------Expect------\n\t13\n\t14\n\t15\n\t16\n\t17\n\t18\n\t19\n\t20\n\t21");]

let whileBlockTest2 = [Debug("Test 5: While-Loop Test w/ Break and Continue");Assign(("i"), Op2("-", Num(5.), Num(4.)));
                        While(Op2("<", Var("i"), Num(10.)), [Assign("i", (Op1("++", Var("i"))));
                                                             Print("print", Var("i"));
                                                             If((Op2("==", Var("i"), Num(5.)), [Break], [Continue]));
                                                             Assign("i", (Op1("--", Var("i"))));
                                                             Print("print", Var("i"));]);Debug("------Expect------\n\t2\n\t3\n\t4\n\t5")]

let whileBlockTest3 = [Debug("Test 6: Nested While-Loop Test");While((Op2("<", Var("i"), Num(5.))), [While((Op2("<", Var("j"), Num(3.))), 
                                                            [Print("print", Var("j")); Assign("j", (Op1("++", Var("j"))))]);
                                                            Assign("j", Num(1.0));
                                                            Assign("i", (Op1("++", Var("i"))))]);
                                                            Debug("------Expect------\n\t0\n\t1\n\t2\n\t1\n\t2\n\t1\n\t2\n\t1\n\t2\n\t1\n\t2\t")]

let forBlockTest1 = [Debug("Test 3: For-Loop Test");For((Assign("i", Num(1.))), 
                        (Op2("<", Var("i"), Num(5.))), 
                        (Assign("i", (Op1("++", Var("i"))))), 
                        [Print("print", Var("i"));
                         Assign("v", Num(15.));
                         Continue;
                         Print("print", Num(69.))]); Print("print", Var("i"));
                                                     Print("print", Var("v"));Debug("------Expect------\n\t1\n\t2\n\t3\n\t4\n\t5\n\t0");]  
                                                
let ifBlock = [Debug("Test 2: Conditional Test");If(Op2("==", Num(1.), Num(1.)), [Assign("x", Num(10.))], [Assign("x", Num(15.))]);
                Print("print", Var("x"));Debug("------Expect------\n\t10");]


let testBlock = [Debug("Test 1: General Basic Functionality Test");Assign("i", Num(1.)); Assign("i", Op1("++", Var("i"))); Print("print", Var("i")); Debug("------Expect------\n\t2");Print("print", Op2("!=", Var("i"), Num(3.)));Debug("------Expect------\n\t1"); ]

let blocks = [testBlock;ifBlock;forBlockTest1;whileBlockTest1;whileBlockTest2;whileBlockTest3;factorialBlock;functionBlock;boolBlock;mathBlock;functionBlock2];;

let rec run (_b: block list) = prints "";
    match _b with
        | [] -> ()
        | a::tl -> runCode a; run (pop _b);;

let main = run blocks
