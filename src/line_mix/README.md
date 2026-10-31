An implemetation of a mixture of linear and nonlinear types. 

Each linear variable must be used exactly once, unrestricted variables (variables that have Bang type `!A`) can be used any number of times.

The language has the following constructs:

- Integers with arithmetic operations `+`, `-`, `*`, `/` and `mod`
- Booleand with conditional statment, comparison of integers `<` and equality comparison `=` for
    - integers
    - booleans
    - pairs
    - sums
    - arrays
- pair type `(a, b) : A * B`.
    Both elements in term `t : A * B` have to be used
    ```
    match t with (x, y) -> f x y
    ```
- sum type `inl a : A + B`, `inr b : A + B`. When creating a sum type, the type must be delcared `(inl x : A + B)`.
    ```
    match t with inl x -> f x | inr y -> g y
    ```
- with type `a & b : A & B`. Zou can use either frst (`fst t`) or the second element (`snd t`), not both. 

- (linear) function type `fun x : A -> e : A -o B`. It can be applied `f a`.
- restriction of variables with `^`. For example, `5 : !Int` and `^5 : Int`
- toplevel definitions with `let` where you can declare variables
    ```
    let a = 3
    let f = fun x : Int -> x + 1
    ```
- recursive functions with `let rec`. If a function uses no linear variable from surrounding context (so it has a type `!(A -o B)`) it can be called as a recursive function.
    ``` 
    let rec f = fun x : !Int -> if x < 10 then f (x + 1) else x
    ```