## Re-Submission of New Version

This is an updated version of the greenclust pacakge currently on CRAN.

The following issue has been FIXED since the previous submission attempt in October 2019:

* noLD "Additional Issue" (a failure running one of the test_that() tests when long doubles are disabled)


## Test environments

This package has been recently tested under:

* Mac OS X 10.12.6 local install: R 3.6.2
* win-builder: Windows Release, x86_64-w64-mingw32 (64-bit), R version 3.6.2 (2019-12-12)
* win-builder: Windows Dev, x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2020-01-03 r77630)
* r-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* r-hub: Ubuntu Linux 16.04, x86_64-pc-linux-gnu (64-bit), R version 3.6.1 (2019-07-05)
* r-hub: Fedora Linux, x86_64-pc-linux-gnu (64-bit), R Under development (unstable) (2020-01-03 r77629)


## R CMD check results

#### Local results:

0 ERRORs, 0 WARNINGs, 0 NOTEs

R CMD check succeeded


#### Win-builder & R-hub results:

All online checkers returned 0 ERRORS and 0 WARNINGS

All release platforms and R-hub Windows Server Dev returned 0 NOTES
  
--------
  
There was 1 NOTE from the R-hub Fedora Linux test:
  
```
> Possibly mis-spelled words in DESCRIPTION:
>  Greenacre (9:18)
>  Greenacre's (3:33)
>  iteratively (6:37)
```
    
These words are spelled correctly
    
--------

  
There was 1 NOTE from the Win-builder Windows Dev test:
  
```
> Found the following (possibly) invalid URLs:
>   URL: https://doi.org/10.1007/BF01901670
>     From: man/greenclust.Rd
>           man/greencut.Rd
>           man/greenplot.Rd
>           README.md
>     Status: Error
>     Message: libcurl error code 35:
>       	error:1407742E:SSL routines:SSL23_GET_SERVER_HELLO:tlsv1 alert protocol version
```

This URL is working and valid


## Downstream dependencies

There are currently no downstream dependencies for this package
