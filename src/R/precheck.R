#!/usr/bin/env Rscript

precheck <- function(workshop_dir) {
    # check maxent
    maxent_jar_file <- file.path(system.file(package='dismo'), '/java/maxent.jar')
    if (! file.exists(maxent_jar_file)) {
        message_base <- 'Maxent.jar is missing. please make sure it is setup with right premission. If maxent.jar not setup, user can download form http://www.cs.princeton.edu/~schapire/maxent/ then put it to %s'
        message <- sprintf(message_base,
                           maxent_jar_file)
        stop(message)
    }
}
