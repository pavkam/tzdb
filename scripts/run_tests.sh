#!/usr/bin/env bash

MODE=${1:-plain}
TEST_OUTPUT_FILE=test_output.txt
if [ "$MODE" = "full" -o "$MODE" = "ci" -o "$MODE" = "plain" ]; then
  echo "🔍 Running tests in '$MODE' mode..."
else
  echo "❌ Invalid mode: '$MODE'"
  exit 1
fi

# Create output directory
mkdir -p ./bin

# Compile and run tests, processing output directly
echo "🔨 Compiling tests..."
fpc ./src/TZTest/TZTest.dpr -Fu./src/TZDBPK -Fu./src/TZDBLIB -FE./bin -FU./bin > $TEST_OUTPUT_FILE 2>&1
if [ $? -ne 0 ]; then
  echo "❌ Failed to compile tests:"
  if [ -s $TEST_OUTPUT_FILE ]; then
    echo "::error::$(cat $TEST_OUTPUT_FILE)"
  fi
  exit 1
else
  echo "✅ Compiled tests"
fi

echo "🔍 Running tests..."

# Handle different output formats based on argument
case "$MODE" in
 "full")
    ./bin/TZTest --all --format=plain --progress
    exit $?
    ;;
  *)
    ./bin/TZTest --all --format=xml > $TEST_OUTPUT_FILE
    ;;
esac

FAILED=0
if [ $? -ne 0 ]; then
  echo "❌ Failed to run tests:"
  FAILED=1
else
  echo "✅ Ran tests, no failures"
fi

# Only parse output for ci format
if [ "$MODE" = "ci" ]; then
  # Extract test results from XML (for CI)
  current_suite=""
  grep -o '<TestSuite Name="[^"]*"\|<Test Name="[^"]*" Result="[^"]*"' $TEST_OUTPUT_FILE | while IFS= read -r line; do
      if [[ $line =~ TestSuite ]]; then
          current_suite=$(echo "$line" | sed -n 's/.*Name="\([^"]*\)".*/\1/p')
      else
          test_name=$(echo "$line" | sed -n 's/.*Name="\([^"]*\)".*/\1/p')
          result=$(echo "$line" | sed -n 's/.*Result="\([^"]*\)".*/\1/p')

          if [[ $result == "OK" ]]; then
              echo "::group::$current_suite - $test_name"
              echo "✅ $test_name passed"
              echo "::endgroup::"
          else
              echo "::group::$current_suite - $test_name"
              echo "❌ $test_name failed"
              echo "::error::$result"
              echo "::endgroup::"
          fi
      fi
  done
elif [ "$MODE" = "plain" ]; then
  # Extract test results from XML (for plain output)
  current_suite=""
  grep -o '<TestSuite Name="[^"]*"\|<Test Name="[^"]*" Result="[^"]*"' $TEST_OUTPUT_FILE | while IFS= read -r line; do
      if [[ $line =~ TestSuite ]]; then
          current_suite=$(echo "$line" | sed -n 's/.*Name="\([^"]*\)".*/\1/p')
          echo "📦 $current_suite"
      else
          test_name=$(echo "$line" | sed -n 's/.*Name="\([^"]*\)".*/\1/p')
          result=$(echo "$line" | sed -n 's/.*Result="\([^"]*\)".*/\1/p')

          if [[ $result == "OK" ]]; then
              echo "   ✅ $test_name"
          else
              echo "   ❌ $test_name"
          fi
      fi
  done
fi

rm -f $TEST_OUTPUT_FILE

exit $FAILED