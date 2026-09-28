#!/usr/bin/env bash
# Runs every tests/*.line_mix program and compares its output with tests/*.expected.
#
#   ./run_tests.sh                 run all tests
#   ./run_tests.sh pairs sums      run only the named tests
#   ./run_tests.sh --update [...]  overwrite .expected files with the current output
#
# The compared output is stdout and stderr together, followed by "[exit N]" when the exit code is not 0.

cd "$(dirname "$0")" || exit 1
root=../../..
exe="$root/_build/default/src/line_mix/line_mix.exe"

update=false
if [ "$1" = "--update" ]; then
  update=true
  shift
fi

dune build --root "$root" src/line_mix/line_mix.exe 2>/dev/null || { echo "build failed"; exit 1; }

if [ $# -gt 0 ]; then
  tests=("$@")
else
  tests=()
  for f in *.line_mix; do tests+=("${f%.line_mix}"); done
fi

run() {
  "$exe" "$1.line_mix" 2>&1
  code=$?
  [ $code -ne 0 ] && echo "[exit $code]"
}

passed=0
failed=()
for t in "${tests[@]}"; do
  if [ ! -f "$t.line_mix" ]; then
    echo "no such test: $t"
    failed+=("$t")
    continue
  fi
  actual=$(run "$t")
  if $update; then
    printf '%s\n' "$actual" > "$t.expected"
    echo "updated $t"
  elif [ ! -f "$t.expected" ]; then
    echo "FAIL $t (missing $t.expected)"
    failed+=("$t")
  elif diff -u --label "$t.expected" --label "actual" "$t.expected" <(printf '%s\n' "$actual") > /dev/null; then
    passed=$((passed + 1))
  else
    echo "FAIL $t"
    diff -u --label "$t.expected" --label "actual" "$t.expected" <(printf '%s\n' "$actual") | sed 's/^/    /'
    failed+=("$t")
  fi
done

$update && exit 0
echo
echo "$passed passed, ${#failed[@]} failed"
[ ${#failed[@]} -eq 0 ]
