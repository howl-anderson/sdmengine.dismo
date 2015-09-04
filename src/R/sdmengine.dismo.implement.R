#!/usr/bin/env Rscript

#' sdmengine.dismo.implement
#'
#' entry interface of implement
#'
#' @param workshop_dir directory of workshop
#' @return NULL
#'
#' @export
sdmengine.dismo.implement <- function(workshop_dir) {
    # prepare logging
    #logging_file = file.path(workshop_dir, "log", "dismo.log")
    #flog.appender(appender.file(logging_file))
    #flog.appender(appender.console())
    #flog.info("logging start")
    output_message('Engine start!')

    configure <- load_configure_file(workshop_dir)

    SDMengine(workshop_dir, configure)

    return(NULL)
}
