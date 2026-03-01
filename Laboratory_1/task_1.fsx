open System.Numerics

let rec findEPS eps =
    if 1.0 + (eps / 2.0) <= 1.0 then eps
    else findEPS(eps / 2.0)

let eps = findEPS 1.0
printfn "Машинное эпсилон (double): %g" eps

let iter a b f i =
    let rec iter' a f acc =
        if a>b then acc
        else iter' (a+1) f (f a acc)
    iter' a f i

// predicate  - функция-условие (возвращает true/false)
// transform  - функция, которая меняет значение на каждом шаге
// startValue - начальное состояние
let rec myWhile predicate transform state =
    if predicate state then state
    else myWhile predicate transform (transform state)



let fact =
    let rec fact' acc = function
        | n when n=1I || n = 0I -> acc
        | n -> fact' (acc * n) (n - 1I)
    fact' 1I


// let power x n = iter 1 n (fun _ acc -> acc * x) 1.0

let naiveSolution x eps =
    // let term n = (power (2.0 * x) n) / float (fact (BigInteger(n)))
    let term (n: int) = (2.0 * x) ** float n / (float (fact (BigInteger(n))))
    let predicate (sum, n) = abs (term n) < eps
    let transform (sum, n) = sum + term (n + 1), n + 1
    let (sum, n) = myWhile predicate transform (1.0, 0)
    (sum, n)


let smartSolution x eps =
    let transform (sum, term, n) =
        let newTerm = term * (2.0 * x) / float n
        (sum + newTerm, newTerm, n + 1)
    let predicate (sum, term, n) = abs term < eps
    let init = (1.0, 1.0, 1)
    let (sum, _, n) = myWhile predicate transform init
    (sum, n-1)



printfn "Вычисление с помощью встроенных функций F#"
for x in [0.1; 0.2; 0.3; 0.4; 0.5; 0.6] do
    printfn "x: %f, результат: %f" x (exp (2.0 * x))


printfn "Вычисление наивным способом вычисления ряда Тейлора,\nгде каждый член ряда вычисляется по формуле"
for x in [0.1; 0.2; 0.3; 0.4; 0.5; 0.6] do
    let (res, n) = naiveSolution x eps
    printfn "x: %f, результат: %f, количество членов ряда: %d" x res n

printfn "Вычисление умный способ вычисления ряда Тейлора,\nгде каждый следующий член вычисляется на основе предыдущего."
for x in [0.1; 0.2; 0.3; 0.4; 0.5; 0.6] do
    let (res, n) = smartSolution x eps
    printfn "x: %f, результат: %f, количество членов ряда: %d" x res n