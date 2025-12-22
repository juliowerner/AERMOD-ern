# AERMOD-ern

This repository contains a Fortran 90 rewrite of the AERMOD atmospheric dispersion model, originally developed by the U.S. Environmental Protection Agency (EPA).
The main goal of this project is to remove the fear to change the code and improve developer experience and code maintainability.

**Secondary goals:**

- Create comprehensive tests to validate the implementation.
- Improve code readability and structure to enable future parallelization.
- Enhance performance while preserving the original functionality of AERMOD.

## Testing

Snapshot tests are provided to compare results with AERMOD versions 23132 and 24142.

To run the tests:

```sh
cd test
./test.sh
```

## TODO

- [x] Create automated tests comparing results between AERMOD versions.
- [ ] Convert source code from fixed format to free format.
- [ ] Standardize variables and keywords to lowercase for consistency.
- [ ] Remove all GOTO statements and replace them with structured programming constructs.
