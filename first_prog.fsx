printfn "Hello World from F#"

let twice x = x * 2

// twice <| 5 |> twice


let solve (a, b, c) =
    let d = b*b - 4.*a*c
    let x1 = (-b+sqrt(d))/2.*a
    let x2 = (-b-sqrt(d))/2.*a
    (x1, x2)


solve(1,2,-3)


// let f = (fun x->x*2) >> (fun x->x*3)


let max x y = if x > y then x else y

let min x y = if x < y then x else y


max 5 7
min 5 7


let a3 f a b c= f (f a b) c

a3 max 6 3 7
a3 min 6 3 7



let rec rpt n f =
    if n=0 then fun x->x
    else f >> rpt (n-1) f


rpt 8 twice 1


// Базовые комбинаторы

// let I x = x // (единичный комбинатор)
// let K x y = X // (канцелятор, вычеркиватель)
// let S x y z = x z (y z) // (распределитель)
// let flip f x y = f y x // (смена порядка арг.)


// let rec iter f i a b =
//     if a <= b then
//         f (iter f i (a + 1) b) a
//     else i


// iter (fun acc i -> acc + i) 0 1 3

// let fact = iter (*) 1 1 

// let power x = iter (fun acc _ -> acc*x) 1. 1

// let myexp x = iter (fun acc i -> acc + (power x i) / (fact i|>float)) 0. 0 7

// power

// myexp 1.


open System.Numerics
let fact =
    let rec fact' acc = function
        | n when n=1I -> acc
        | n -> fact' (acc * n) (n - 1I)
    fact' 1I

fact 50I


let iter a b f i =
    let rec iter' a f acc =
        if a>b then acc
        else iter' (a+1) f (f a acc)
    iter' f i