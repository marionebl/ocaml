#!/bin/bash
set -e

if output=$(git status --porcelain -- "exercises/**/test.ml") && [ -z "$output" ]; then
  echo "Tests are in sync."
else
  echo "Checked in test files diverged from generated files:"
  echo $output
  exit 1
fi