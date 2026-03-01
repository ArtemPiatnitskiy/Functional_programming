let eps = 1e-6

let equation20 x = 0.1 * x ** 2.0 - x * log(x)
let equation21 x =tan x - (tan x)**3.0/3.0 + (tan x)**5.0/5.0 - 1.0/3.0
let equation22 x = acos x - sqrt(1.0 - 0.3 * x**3.0)

let eq20forIter x = exp (0.1 * x)
let eq21forIter x = 1.0/3.0
let eq22forIter x = cos(sqrt(1.0 - 0.3 * x**3.0))

let rec dehatomy f a b eps =
    let c = (a + b) / 2.0
    if abs(b - a) < eps then c
    elif (f a) * (f c) < 0.0 then dehatomy f a c eps
    else dehatomy f c b eps

let rec iteration g x eps =
    let next = g x
    if abs(next - x) < eps then next
    else iteration g next eps

let newton f x0 eps =
    let h = sqrt eps
    let df x = (f (x + h) - f x) / h
    let g x = x - f x / df x
    iteration g x0 eps



// dehatomy equation20 1.0 2.0 eps
// dehatomy equation21 0.0 0.8 eps
// dehatomy equation22 0.0 1.0 eps

// iteration eq20forIter 1.5 eps
// iteration eq21forIter 0.4 eps
// iteration eq22forIter 0.5 eps

// newton equation20 1.5 eps
// newton equation21 0.4 eps
// newton equation22 0.5 eps


let separator = sprintf "+%s+%s+%s+" (String.replicate 12 "-") (String.replicate 6 "-") (String.replicate 12 "-")

printfn "%s" separator
printfn "| %-10s | %-4s | %-10s |" "Метод" "Ур-е" "Результат"
printfn "%s" separator
printfn "| %-10s | %-4d | %-10.6f |" "Дихотомия" 20 (dehatomy equation20 1.0 2.0 eps)
printfn "| %-10s | %-4d | %-10.6f |" "Дихотомия" 21 (dehatomy equation21 0.0 0.8 eps)
printfn "| %-10s | %-4d | %-10.6f |" "Дихотомия" 22 (dehatomy equation22 0.0 1.0 eps)
printfn "%s" separator
printfn "| %-10s | %-4d | %-10.6f |" "Итерация" 20 (iteration eq20forIter 1.5 eps)
printfn "| %-10s | %-4d | %-10.6f |" "Итерация" 21 (iteration eq21forIter 0.4 eps)
printfn "| %-10s | %-4d | %-10.6f |" "Итерация" 22 (iteration eq22forIter 0.5 eps)
printfn "%s" separator
printfn "| %-10s | %-4d | %-10.6f |" "Ньютон" 20 (newton equation20 1.5 eps)
printfn "| %-10s | %-4d | %-10.6f |" "Ньютон" 21 (newton equation21 0.4 eps)
printfn "| %-10s | %-4d | %-10.6f |" "Ньютон" 22 (newton equation22 0.5 eps)
printfn "%s" separator