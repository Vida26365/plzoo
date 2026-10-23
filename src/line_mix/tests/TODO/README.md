# TODO: known bugs

Each file is a minimal program that shows one bug. The comment at the top says
what happens, what should happen, and where in the code to look.

Run one with: `./line_mix.exe -n src/line_mix/tests/TODO/<file>`

| file | area | short description |
|---|---|---|
| 01_annot_sum | type checker | `(inl 1 : Int + Bool)` fails, annotation is ignored |
| 02_eq_with_crash | interpreter | `(1 & 2) = (1 & 2)` hits `assert false` |
| 03_eq_inl_inr | interpreter | `inl = inr` hits `assert false` (needs 01 first) |
| 04_if_branch_context | linearity | else branch can't use what then branch used |
| 05_with_context | linearity | second half of `&` can't use what first half used |
| 06_fun_captures_linear | linearity | function using an outer linear variable can be called twice |
| 07_array_check_context | linearity | checked array literal doesn't use up its elements |
| 08_make_duplicates_linear | linearity | `make 2 f` copies a file handle |
| 09_lookup_duplicates_linear | linearity | `lookup` duplicates a linear element |
| 10_let_rec_not_function | error message | `let rec f : Int = 5` says only "TODO" |
| 11_runtime_error_fatal | error handling | `1 / 0` crashes with an OCaml exception |

When a bug is fixed, move its file to `pass/` or `fail/` (with an `.expected`
file or a `# expect:` line) so it stays tested.
