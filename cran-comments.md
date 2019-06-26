## Test environments
* OS X local install: R 3.6.0
* win-builder: x86_64-w64-mingw32 (64-bit), R-devel (2019-06-23 r76735)
* r-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* r-hub: Ubuntu Linux 16.04 LTS, R-release, GCC
* r-hub: Fedora Linux, R-devel, clang, gfortran
   
## R CMD check results

0 ERRORs, 0 WARNINGs, 0 NOTEs

R CMD check succeeded

#### Win-builder & R--hub notes:

All online checkers returned the same note:
    
------
    
> Status: 1 NOTE "Possibly mis-spelled words in DESCRIPTION"
>    
>  * Greenacre's (3:33)
>  * hclust (9:46)
>  * iteratively (6:37)
    
--------
    
These words are spelled correctly

## Downstream dependencies

There are currently no downstream dependencies for this package
