(*open Core*)

let globalScope = Stack.create ()

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 

type block = statement list 

type env = N of float (* complete *)

type envQueue = env list

let varEval (_v: string) (_q: envQueue): float  = 0.0  

let evalOp1 (_v: string) (_e: expr) (_q: envQueue) = 0.0 

let evalOp2 (_v: string) (_l: expr) (_r: expr) (_q: envQueue) = 0.0 

let evalFct (_v: string) (_e: expr list) (_q: envQueue) = 0.0

let evalExpr (_e: expr) (_q: envQueue): float  = 
    match _e with    
        | Num(x) -> x
        | Var(x) -> varEval x _q
        | Op1(str, x) -> evalOp1 str x _q
        | Op2(str, x, y) -> evalOp2 str x y _q
        | Fct(str, [x]) -> evalFct str [x] _q
        | _ -> 0.0 (*some kind of error here *)

(* Test for expression *)
(*let%expect_test "evalNum" = 
    evalExpr (Num 10.0) [] |>
    printf "%F";
    [%expect {| 10. |}]*)

let evalCode (_code: block) (_q: envQueue): unit = 
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    Stack.push (Hashtbl.create 123456) globalScope
    (*let scope = Stack.top globalScope in
        let eval = evalStatement Assign("v" (Num(5))) scope*)
    (*print_endline "does this do something?"*)


let evalStatement (s: statement) (q: envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> (* eval e and store in v *) q
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalCode codeT q 
                else
                    evalCode codeF q
            ;q
        | _ -> q (*ignore *)


(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

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
]

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
let p3: block = 
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
    ]

(*let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]*)

let test = (evalExpr (Num(3.)) [])


let main = 
    print_float (evalExpr (Num(3.)) [])