#open Core.Std

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr =
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string*list expr

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list*statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string*string list*

type env = float String.Map.t 

type envQueue = nev list

let varEval (v: string) (q: envQueue): float 

let evalExp (e: expr)

let evalCode (code: statement list) (q: envQueue): unit
    (*create new enviornment*)
    (*use fold_left*)
    (*pop the local enviornment*)

let evalStatement (v: string) (q: envQueue): envQueue =
    match s with 
        | Assign(v, e) -> (*evaluate and store in v*)
        | If(e, codeT, codeF) -> 
            let cond = evalExp e q in 
                if(cond > 0) then 
                    evalCode codeT q
                else
                    evalCode codeF 
                    
(*ppx_inline_test/
    dune *)