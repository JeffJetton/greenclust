# greenclust 1.1.1

* Fixes error when running testthat tests on some platforms, due to an incorrect version argument type
* Internally converts integer matrices to doubles to avoid potential overflow
  

# greenclust 1.1

* The core clustering code has been completely re-written and is now far more efficient.
    * Clustering is typically about ten times faster now.
    * Due to changes in how chi-squared calculations are now made and combined clusters are tracked along the way, potential row combinations that are "tied" (or have chi-squared reductions that are so extremely close as to be essentially tied) may be clustered in a different order compared to the version 1.0 function.
        * This generally only affects earlier clustering steps, well above where most cut points will be in practice.
        * If results from a previous version 1.0 clustering need to be reproduced exactly for every step, you can force the use of the old function by calling `greenclust:::.greenclust.v1()`. This feature is not guaranteed to remain in future versions.

* Verbose output now includes the names of the two rows that were combined at each step.

* Fix for passing data frames to `greenclust()` (#8)
