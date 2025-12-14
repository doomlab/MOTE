## Test environments
* Local macOS Tahoe, R 4.4.1
* GitHub Actions:
  - ubuntu-latest (R-release, R-devel)
  - macOS-latest (R-release)
  - windows-latest (R-release)
* win-builder (R-release, R-devel)

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Notes

* This is an update of the existing CRAN package **MOTE**.
* The package now includes a modernization of internal code, documentation, and effect size functionality.
* The update introduces *new snake_case function names*, while **fully preserving backward compatibility** by keeping all previously exported dotted-name functions (e.g., `eta.partial.SS`). No existing user code should break.
* The package no longer depends on `MBESS` or `ez`, which reduces external dependencies and improves installation reliability.
* A full set of unit tests has been added using **testthat**, providing >95% coverage across all effect size functions.
* All examples run quickly, and documentation has been updated to meet current CRAN standards.

## revdepcheck results

* Issue with tabledown discovered and fixed. 

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Additional comments

This update substantially improves:
* readability and consistency of the API
* consistency of output structures
* calculation clarity for effect sizes and confidence intervals
* long-term maintainability of the package

Thank you for your time and consideration.
