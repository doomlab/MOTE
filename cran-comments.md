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

## Reverse dependencies

There is 1 reverse dependency. No breaking changes were introduced because:
* All existing exported functions remain available under their original dotted names.
* No arguments or return values of legacy functions were removed.
* Only new, additional snake_case APIs were added.

Reverse dependency checks show no issues.

## Additional comments

This update substantially improves:
* readability and consistency of the API
* consistency of output structures
* calculation clarity for effect sizes and confidence intervals
* long-term maintainability of the package

Thank you for your time and consideration.
