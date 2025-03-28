# Get mode from first argument, default to "plain"
$mode = if ($args[0]) { $args[0] } else { "plain" }
$testOutputFile = "test_output.txt"

# Validate mode
if ($mode -notin @("plain", "full", "ci")) {
    Write-Host "❌ Invalid mode: '$mode'"
    exit 1
}

Write-Host "🔍 Running tests in '$mode' mode..."

# Create bin directory if it doesn't exist
New-Item -ItemType Directory -Force -Path "bin" | Out-Null

# Compile the test program
Write-Host "🔨 Compiling test program..."
fpc "./src/TZTest/TZTest.dpr" -Fu"./src/TZDBPK" -Fu"./src/TZDBLIB" -FE"./bin" -FU"./bin" > $testOutputFile 2>&1

if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to compile tests:"
    if (Test-Path $testOutputFile) {
        Get-Content $testOutputFile
    }
    exit 1
} else {
    Write-Host "✅ Compiled tests"
}

Write-Host "🔍 Running tests..."

# Handle different output formats based on mode
$failed = 0
switch ($mode) {
    "full" {
        ./bin/TZTest --all --format=plain --progress
        $failed = $LASTEXITCODE
    }
    default {
        ./bin/TZTest --all --format=xml > $testOutputFile 2>&1
        $failed = $LASTEXITCODE
    }
}

if ($failed -ne 0) {
    Write-Host "❌ Failed to run tests:"
} else {
    Write-Host "✅ Ran tests, no failures"
}

# Process output based on mode
if ($mode -eq "ci") {
    # Extract test results from XML (for CI)
    $currentSuite = ""
    Get-Content $testOutputFile | ForEach-Object {
        if ($_ -match '<TestSuite Name="([^"]+)"') {
            $currentSuite = $matches[1]
        }
        elseif ($_ -match '<Test Name="([^"]+)" Result="([^"]+)"') {
            $testName = $matches[1]
            $result = $matches[2]
            Write-Host "::group::$currentSuite - $testName"
            if ($result -eq "OK") {
                Write-Host "✅ $testName passed"
            } else {
                Write-Host "❌ $testName failed"
                Write-Host "::error::$result"
            }
            Write-Host "::endgroup::"
        }
    }
} elseif ($mode -eq "plain") {
    # Extract test results from XML (for plain output)
    $currentSuite = ""
    Get-Content $testOutputFile | ForEach-Object {
        if ($_ -match '<TestSuite Name="([^"]+)"') {
            $currentSuite = $matches[1]
            Write-Host "📦 $currentSuite"
        }
        elseif ($_ -match '<Test Name="([^"]+)" Result="([^"]+)"') {
            $testName = $matches[1]
            $result = $matches[2]
            if ($result -eq "OK") {
                Write-Host "   ✅ $testName"
            } else {
                Write-Host "   ❌ $testName"
            }
        }
    }
}

# Clean up
Remove-Item -Force $testOutputFile -ErrorAction SilentlyContinue

exit $failed
