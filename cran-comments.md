## Resubmission
This is a resubmission. I have made these requested fixes to the DESCRIPTION file's Description field:

* Added single quotes around package and function names
* Added parentheses to the end of the function name `hclust()`
* Included a citation, using author name, year, and doi

I have also taken advantage of the resubmit to add URL and BugReports fields to the DESCRIPTION file.
  
## Test environments
* OS X local install: R 3.6.0
* win-builder: x86_64-w64-mingw32 (64-bit), R-devel (2019-06-27 r76748)
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
>  * Greenacre (9:18)
>  * Greenacre's (3:33)
>  * iteratively (6:37)

--------
    
These words are spelled correctly

## Downstream dependencies

There are currently no downstream dependencies for this package
