// 1. Feito por Gabriel Sulzbach Silveira.
let rec produtoImpares lista =
    match lista with
    | [] -> 1
    | head::tail when head % 2 <> 0 -> head * produtoImpares tail
    | _::tail -> produtoImpares tail

// 2. Feito por Jordan Lippert.
let rec exercicioDois num1 num2 =
    let rec repetirMultiplicacao numeroBase exponente resultado =
        match exponente with
        | 0 -> resultado
        | _ -> repetirMultiplicacao numeroBase (exponente - 1) (resultado * numeroBase)

    match num1 with
    | x when x = num2 -> num1 * num2
    | _ -> repetirMultiplicacao num1 num2 1

// 3. Feito por Caio Furtado Rosa.
let rec isPrime number =
    let rec checkDividers n =
        match n with
        | x when x >= number -> true
        | x when (number % x) = 0 -> false // Possui divisor
        | _ -> checkDividers (n + 1) // Verificar próximo divisor

    match number with
    | 1 -> false
    | 2 -> true
    | x when x > 2 -> checkDividers 2
    | _ -> false
    
// 4. Feito por Caio Furtado Rosa.
let rec sumPrimes list =
    match list with
    | [] -> 0
    | head::tail when isPrime head -> head + sumPrimes tail // Se o número atual é primo, somar com a soma dos primos da tail
    | _::tail -> sumPrimes tail // Ignora o número atual porque não é primo

let testList = [10; 5; 7; 12; 6; 9; 32; 2]

let resultado1 = produtoImpares testList
let resultado2 = sumPrimes testList
let resultado3 = isPrime 5
let resultado4 = exercicioDois 2 3

printf "\nLista de teste: %A\n" testList
printfn "\nProduto dos numeros impares: %d" resultado1
printfn "\nVerificar se é primo: %b" resultado3
printfn "\nSomar os primos da lista: %A" resultado2
printfn "\nResultado exercicio 2: %A" resultado4