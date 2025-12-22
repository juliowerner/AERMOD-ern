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

## Steps taken so far

- [x] Create automated tests comparing results between AERMOD-ern and AERMOD 23132/24142.
- [x] Convert source code from fixed format to free format using `findent'

## TODO

- [ ] Standardize variables and keywords to lowercase for consistency.
- [ ] Remove all GOTO statements and replace them with structured programming constructs.

# Acknowledgements

- US EPA for developing theo original [AERMOD model](https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models#aermod).
- The developers of:
  - `findent` for providing a useful tool to convert Fortran code from fixed to free format. [SourceForge](https://sourceforge.net/projects/findent/)
  - `fortitude` for the linter used to improve code quality and automatically fix some issues. [GitHub](https://github.com/PlasmaFAIR/fortitude)
  - `flowercase.py` for the tools to change case of variables and keywords. [GitHib](https://github.com/ylikx/fortran-legacy-tools)
