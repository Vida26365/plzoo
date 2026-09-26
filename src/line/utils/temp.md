1. Kako pognati `example.line`

2. Kaj je `{ VAR (lexeme lexbuf) }`

3. Kaj je potrebno narediti v `line.ml`
let exec _ _ = initial_environment (* TODO *)
- type checker





# Kaj dela kaj:

- Interpret.ml:
    - delo z okolji




# TODO

- Pair ne potrebje environment
- assert false namesto errorjev

## Kako nardim typechecker

- kontekst: spremenljivke -> tipe
- pri funkcijah pišep tipe (lahko samo tp argumenta, če ni rekurzije)
- KOnetekst


## POtem
- datoteke
- inti so nelinearni
- klicaj




let sumT = if 5 < 6 then inl 1 else inr (2, 3);

match sumT with inl x = x | inr pair = (split pair to a b in a*b);