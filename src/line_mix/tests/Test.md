# What do we want to test for

### interpreter
- Arithmetic works as expected
    - devision by zero
    - mod devision by zero
    - +, -, *, /, mod
- Arrays work as expected
    - Adding to array
    - Looking at array and setting
        - List index out of bounds
- Match
- Each word from `lexer.mll` should appear at least once

### Type Checker



# Vprašanja

- `LBRACKET es = separated_list(COMMA, expr) RBRACKET { Array es }`  -- Ali je verižni seznam vredu
- Ali moram dodati $i<n$ pri set in lookup
- Ali je vredu, da var ne potrebuje vsega konteksta
- program napisan v zvezku
- Kako naj bo strukturirana moja diplomska?
    - opis naravne dedukcije in curry howard
    - kaj je linearna logika
    - opis programa
    - implementacija