# AERMOD-ern

This repository contains a Fortran 90 rewrite of the AERMOD atmospheric dispersion model, originally developed by the U.S. Environmental Protection Agency (EPA). 
The main goal of this project is to remove the fear to change the code and improve developer experience and code maintainability.

Secondary goals include:
1. Create tests to validate the implementation
2. Improve code readability to improve parallelization capabilities.
3. Enhance performance while preserving the original functionality of AERMOD.

# TODO
- [ ] Create automatic tests comparing results between Aermod versions.
- [ ] Convert from fixed format to free format.
- [ ] Uniformize variables and keyword to lowercase for consistency.
- [ ] Remove all GOTO statements and replace them with structured programming constructs.
