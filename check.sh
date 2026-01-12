#!/bin/bash
# Byte-compilation check for the Emacs config
# Catches syntax errors and many undefined references
#
# Usage: ./check.sh [--verbose]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

VERBOSE=${1:-}

echo "=== Checking Elisp files ==="
echo ""

FILES="autoimport.el elisp.el gptel-custom.el elisp_after_general.el general.el"

ERRORS=0

for file in $FILES; do
    [ ! -f "$file" ] && continue

    echo -n "Checking $file... "

    # Byte-compile and capture output
    OUTPUT=$(emacs --batch \
        --eval "(setq byte-compile-error-on-warn nil)" \
        --eval "(setq load-prefer-newer t)" \
        -f batch-byte-compile "$file" 2>&1)

    # Clean up .elc file
    rm -f "${file}c"

    # Filter out expected "missing package" errors (these are fine for config files)
    REAL_ERRORS=$(echo "$OUTPUT" | grep -E "Error:|error:" | grep -v "Cannot open load file" | grep -v "No such file or directory" || true)

    if [ -n "$REAL_ERRORS" ]; then
        echo "ERROR"
        echo "$REAL_ERRORS"
        ERRORS=$((ERRORS + 1))
    elif echo "$OUTPUT" | grep -q "Cannot open load file"; then
        echo "OK (needs packages)"
        [ -n "$VERBOSE" ] && echo "$OUTPUT" | grep "Cannot open load file" | head -3
    elif echo "$OUTPUT" | grep -q "Warning:"; then
        echo "OK (warnings)"
        [ -n "$VERBOSE" ] && echo "$OUTPUT" | grep "Warning:" | head -5
    else
        echo "OK"
    fi
done

echo ""
if [ $ERRORS -gt 0 ]; then
    echo "=== BYTE-COMPILE FAILED: $ERRORS file(s) with errors ==="
    exit 1
else
    echo "=== Byte-compile: PASSED ==="
fi

echo ""
echo "=== Running ERT tests ==="
echo ""

TEST_OUTPUT=$(emacs -batch -l ert -l elisp.el -l autoimport.el -l test-elisp.el -f ert-run-tests-batch-and-exit 2>&1)
TEST_EXIT=$?

# Show test summary
echo "$TEST_OUTPUT" | grep -E "^(Ran |   passed|   FAILED|Running )"

echo ""
if [ $TEST_EXIT -eq 0 ]; then
    echo "=== ALL CHECKS PASSED ==="
    exit 0
else
    echo "=== TESTS FAILED ==="
    echo "$TEST_OUTPUT" | grep -A5 "FAILED"
    exit 1
fi
