#!/bin/bash
set -e

# Output file for the summary
SUMMARY_FILE=${GITHUB_STEP_SUMMARY:-/dev/stdout}

echo "# ♟️ CI Benchmark Summary" >> "$SUMMARY_FILE"
echo "" >> "$SUMMARY_FILE"
echo "Date: $(date)" >> "$SUMMARY_FILE"
echo "" >> "$SUMMARY_FILE"

echo "## ⚡ Core Performance (Perft)" >> "$SUMMARY_FILE"
echo "| Test Case | Depth | Nodes | Time | NPS |" >> "$SUMMARY_FILE"
echo "|---|---|---|---|---|" >> "$SUMMARY_FILE"

# Run bench-core and parse output
# Expected output format: "Core | Start      | Depth 5 | Nodes:    4865609 | Time:  0.780s | NPS:    6234103"
cabal run bench-core > bench_core.log 2>&1

grep "Core |" bench_core.log | while read -r line; do
    # Extract fields using awk
    NAME=$(echo "$line" | awk -F'|' '{print $2}' | xargs)
    DEPTH=$(echo "$line" | awk -F'|' '{print $3}' | awk '{print $2}' | xargs)
    NODES=$(echo "$line" | awk -F'|' '{print $4}' | awk '{print $2}' | xargs)
    TIME=$(echo "$line" | awk -F'|' '{print $5}' | awk '{print $2}' | xargs)
    NPS=$(echo "$line" | awk -F'|' '{print $6}' | awk '{print $2}' | xargs)

    # Format NPS with commas if possible (using printf)
    printf "| %s | %s | %s | %s | **%s** |\n" "$NAME" "$DEPTH" "$NODES" "$TIME" "$NPS" >> "$SUMMARY_FILE"
done

echo "" >> "$SUMMARY_FILE"
echo "## 🔍 Search Performance" >> "$SUMMARY_FILE"

# Run bench-search
# Output:
# Starting Benchmark (KiwiPete Depth 6)...
# info ...
# Time: 26.86021957s

echo "Running Search Benchmark (KiwiPete Depth 6)..."
cabal run bench-search > bench_search.log 2>&1

# Extract Time
TIME_SEARCH=$(grep "Time:" bench_search.log | awk '{print $2}')
# Extract last info line for Nodes
LAST_INFO=$(grep "info depth" bench_search.log | tail -n 1)
NODES_SEARCH=$(echo "$LAST_INFO" | grep -o 'nodes [0-9]*' | awk '{print $2}')

# Calculate NPS (Nodes / Time)
# Remove 's' from time
TIME_VAL=$(echo "$TIME_SEARCH" | sed 's/s//')
NPS_SEARCH=$(python3 -c "print(f'{int($NODES_SEARCH / float($TIME_VAL)):.0f}')")

echo "- **Scenario**: KiwiPete (Depth 6)" >> "$SUMMARY_FILE"
echo "- **Nodes**: $NODES_SEARCH" >> "$SUMMARY_FILE"
echo "- **Time**: $TIME_SEARCH" >> "$SUMMARY_FILE"
echo "- **NPS**: **$NPS_SEARCH**" >> "$SUMMARY_FILE"

echo "" >> "$SUMMARY_FILE"
echo "✅ Benchmarks Completed" >> "$SUMMARY_FILE"

# Cleanup
rm bench_core.log bench_search.log
