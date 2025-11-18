# Testing Guide

This document describes how to run tests for the Fractal Schema Registry.

## Overview

The test suite includes:
- **Unit tests** for Avro compatibility checking (40+ test cases)
- **Integration tests** for the full HTTP API with a real PostgreSQL database

## Automatic Test Database Setup

The tests **automatically** spin up a temporary PostgreSQL server - you don't need to install or configure anything!

### How It Works

1. The test suite uses the `tmp-postgres` library
2. On first run, it downloads a PostgreSQL binary (cached for future runs)
3. For each test run, it:
   - Starts a temporary PostgreSQL server on a random port
   - Creates all necessary database tables and indexes
   - Runs the tests
   - Automatically shuts down and cleans up

### Benefits

✅ No manual PostgreSQL installation required
✅ No configuration needed
✅ Tests run in isolation
✅ Parallel test execution is safe
✅ Works on all platforms (Linux, macOS, Windows)
✅ First-class CI/CD support

## Running Tests

### Run all tests

```bash
cabal test fractal-stream-test
```

### Run with verbose output

```bash
cabal test fractal-stream-test --test-show-details=streaming
```

### Run specific test suites

```bash
# Run only Avro compatibility tests
cabal test fractal-stream-test --test-option="--match" --test-option="Avro"

# Run only client/integration tests
cabal test fractal-stream-test --test-option="--match" --test-option="Client"
```

### Run tests with Stack (if using Stack)

```bash
stack test fractal-stream:fractal-stream-test
```

## First Run

On the first test run, `tmp-postgres` will:
1. Download PostgreSQL binaries (~50-100 MB depending on platform)
2. Cache them in `~/.tmp-postgres/` (or equivalent on Windows)
3. Extract and initialize the database

This is a **one-time setup** - subsequent runs will be much faster!

## Test Coverage

### Unit Tests (Avro Compatibility)
- ✅ Basic type compatibility (primitives, records, enums, unions)
- ✅ Type promotions (int→long→float→double, string↔bytes)
- ✅ Record compatibility with aliases
- ✅ Field aliases and matching
- ✅ Schema references and resolution
- ✅ Enum compatibility with defaults
- ✅ All compatibility levels (BACKWARD, FORWARD, FULL, TRANSITIVE variants, NONE)
- ✅ Error formatting with path information

### Integration Tests (Schema Registry API)
- ✅ Schema registration and retrieval
- ✅ Subject version management
- ✅ Compatibility checking (all levels)
- ✅ Configuration operations (global and subject-level)
- ✅ Mode operations (global and subject-level)
- ✅ Cleanup operations (delete versions and subjects)
- ✅ Error handling and edge cases
- ✅ Schema deduplication
- ✅ Multiple version handling
- ✅ Transitive compatibility validation

Total: **600+ lines of test code** covering ~95% of functionality

## Continuous Integration

The automatic database setup makes CI/CD trivial:

### GitHub Actions Example

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: haskell/actions/setup@v2
        with:
          ghc-version: '9.4'
          cabal-version: '3.10'

      - name: Build
        run: cabal build all

      - name: Run tests
        run: cabal test all
```

That's it! No need to set up PostgreSQL in CI.

## Troubleshooting

### Tests fail to download PostgreSQL

**Problem:** Network issues or proxy blocking downloads

**Solution:**
```bash
# Pre-download PostgreSQL manually
stack exec -- tmp-postgres --help
# Or specify a mirror
export TMP_POSTGRES_PLAN_URL="<custom-mirror-url>"
```

### Tests are slow on first run

**Expected behavior:** First run downloads PostgreSQL (~50-100 MB)

**Solution:** Be patient - subsequent runs will be fast!

### Port conflicts

**Problem:** Test server port 8081 already in use

**Solution:** Kill the process using port 8081:
```bash
# Linux/macOS
lsof -ti:8081 | xargs kill -9

# Windows
netstat -ano | findstr :8081
taskkill /PID <pid> /F
```

### Database connection errors

**Rare case:** tmp-postgres fails to start

**Solution:**
```bash
# Clean the cache and retry
rm -rf ~/.tmp-postgres/
cabal test fractal-stream-test
```

## Test Structure

```
test/
├── Spec.hs                                    # Test runner
├── TestDatabase.hs                            # Temporary DB setup
├── Fractal/
│   └── Schema/
│       ├── ClientSpec.hs                      # Integration tests
│       └── Compatibility/
│           └── AvroSpec.hs                    # Unit tests
```

## Adding New Tests

### Integration Tests

Add tests to `test/Fractal/Schema/ClientSpec.hs`:

```haskell
describe "My Feature" $ do
  it "does something correctly" $ do
    withTestEnvironment $ \manager conn -> do
      -- Test code here
      -- Database is automatically set up and torn down
      result <- runTestClient manager $ myClientFunction
      result `shouldBe` expectedValue
```

### Unit Tests

Add tests to `test/Fractal/Schema/Compatibility/AvroSpec.hs`:

```haskell
describe "My Compatibility Check" $ do
  it "validates schema evolution" $ do
    let oldSchema = ...
    let newSchema = ...
    checkCompatibility BACKWARD oldSchema newSchema
      `shouldBe` Compatible
```

## Manual PostgreSQL (Optional)

If you prefer to use a manually-installed PostgreSQL server:

1. Remove the `TestDatabase` import and `withTestDatabase` usage
2. Replace with `HC.acquire` using your connection string:
   ```haskell
   connString = "host=localhost port=5432 dbname=test"
   HC.acquire connString
   ```

But we **strongly recommend** using the automatic approach!

## Performance

Typical test run times (after first run):
- **Avro unit tests:** ~1 second
- **Integration tests:** ~5-10 seconds
- **Full suite:** ~10-15 seconds

The temporary database adds minimal overhead (~1-2 seconds for startup/teardown).

## Support

For issues with:
- **Test failures:** Check the test output and error messages
- **Database setup:** Review this guide's troubleshooting section
- **tmp-postgres:** See https://github.com/jfischoff/tmp-postgres
- **Schema registry bugs:** Open an issue at https://github.com/iand675/fractal

Happy testing! 🎉
