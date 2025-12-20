#!/usr/bin/env bash
# Test summary utility - run and parse aiken test output
# Usage: ./test_summary.sh [module_pattern]
# Examples:
#   ./test_summary.sh           # Run all tests
#   ./test_summary.sh pool_scoop # Run tests matching pool_scoop

MODULE_PATTERN="${1:-}"

if [ -n "$MODULE_PATTERN" ]; then
    OUTPUT=$(script -q -c "aiken check -m $MODULE_PATTERN" /dev/null 2>&1)
else
    OUTPUT=$(script -q -c "aiken check" /dev/null 2>&1)
fi

# Strip ANSI codes for parsing
CLEAN=$(echo "$OUTPUT" | sed 's/\x1b\[[0-9;]*m//g')

echo "========================================"
echo "TEST SUMMARY"
echo "========================================"

# Count passes and fails
PASSES=$(echo "$CLEAN" | grep -c "PASS")
FAILS=$(echo "$CLEAN" | grep -c "FAIL")

echo "Passed: $PASSES"
echo "Failed: $FAILS"
echo ""

# Show any failures with context
if [ "$FAILS" -gt 0 ]; then
    echo "FAILED TESTS:"
    echo "----------------------------------------"
    echo "$CLEAN" | grep -A 3 "FAIL"
    echo ""
fi

# Show warnings
WARNINGS=$(echo "$CLEAN" | grep -c "unused\|warning")
if [ "$WARNINGS" -gt 0 ]; then
    echo "WARNINGS ($WARNINGS):"
    echo "----------------------------------------"
    echo "$CLEAN" | grep -B 1 -A 2 "unused"
    echo ""
fi

# Show final summary line
echo "----------------------------------------"
echo "$CLEAN" | grep "Summary"

# Exit with failure code if any tests failed
if [ "$FAILS" -gt 0 ]; then
    exit 1
fi
