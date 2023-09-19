## Version 1.1.1

This is an update to the version 1.1.0 greenclust package currently on CRAN.

The following issue has been FIXED:

*  'LazyData' is specified without a 'data' directory 

In addition, NOTEs from yesterday's 1.1.1 submission have been FIXED:

* Found the following (possibly) invalid URLs:
  URL: https://pdfs.semanticscholar.org/bbb0/2b26cf6a1628b27ddef70a83b92962d6dce2.pdf (moved to https://www.semanticscholar.org/paper/Correcting-the-Quasi-complete-Separation-Issue-in-Lu-Caritas/bbb02b26cf6a1628b27ddef70a83b92962d6dce2?p2df)
    * URL replaced with one that returns a 400 result
* Found the following URLs which should use \doi (with the DOI name only):
    * File 'greenclust.Rd': https://doi.org/10.1007/BF01901670
    * File 'greencut.Rd': https://doi.org/10.1007/BF01901670
    * File 'greenplot.Rd': https://doi.org/10.1007/BF01901670

## Test environments

This package has been very recently tested under:

* Mac OS X 12.5.1 local install
    * R version 4.3.1
* Win-Builder, R-devel
    * R version Under development (unstable) (2023-09-18 r85166 ucrt)
    * Windows Server 2022 x64 (build 20348)
* R-Hub platforms corresponding to flavors giving errors on CRAN with v1.1.0:
    * Debian Linux, R-devel, clang, ISO-8859-15 locale
    * Debian Linux, R-devel, GCC
* Other R-Hub platforms:
    * Debian Linux, R-release, GCC
    * Fedora Linux, R-devel, GCC
    * Ubuntu Linux 20.04.1 LTS, R-release, GCC
    * Windows Server 2022, R-release, 32/64 bit



## R CMD check results

#### Local results:

0 ERRORs, 0 WARNINGs, 0 NOTEs

R CMD check succeeded


#### Win-builder results:

OK

0 ERRORS, 0 WARNINGS, 0 NOTES


#### R-hub results:

0 ERRORS, 0 WARNINGS, 0 NOTES

All platform checks returned OK



## Reverse dependencies

There are currently no known reverse dependencies for this package
