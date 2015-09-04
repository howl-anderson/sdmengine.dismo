#!/usr/bin/env Rscript

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("hello, R user. This is SDM serial software. Have fun with using.")

    return(NULL)
}
