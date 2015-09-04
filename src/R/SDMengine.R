#!/usr/bin/env Rscript

#' make_progress_database
#' @param workshop_dir Directory of workshop
#' @param configure Configure object
make_progress_database <- function(workshop_dir, configure) {
    species_name_vector <- configure[["species_name"]]
    algorithms_list <- configure[["algorithms"]]
    runtimes_number <- configure[["run_times"]]
    predict_environment_set <- configure[["predict"]]

    db_object <- connect_database(workshop_dir)

    if ("engine_progress" %in% dbListTables(db_object)) {
        sql_string <- 'SELECT count(*) FROM engine_progress'
        result_obj <- dbSendQuery(db_object, sql_string)
        result_data <- dbFetch(result_obj)
        dbClearResult(result_obj)
        row_count <- result_data[1, 1]

        if (row_count > 0) {
            output_message("Find progress database, create database is pass")

            dbDisconnect(db_object)
            return(FALSE)
        } else {
            output_message("Start to create progress database")

            data_grid <- expand.grid(species_name=species_name_vector,
                                     algorithms=algorithms_list,
                                     runtime=seq(runtimes_number),
                                     mark=0,
                                     elapsed_senconds=-1)

            data_grid <- as.data.frame(data_grid)

            data_list <- split(data_grid, rownames(data_grid))

            lapply(X=data_list, FUN=function(x) {
                       item <- as.list(x)

                       sql_string_bare <- "INSERT INTO `engine_progress` (`id`, `species`, `algorithm`, `runtime`, `mark`, `elapsed_time`) VALUES (NULL, '%s', '%s', '%s', '%s', '%s');"
                       sql_string <- sprintf(sql_string_bare,
                                             item$species_name,
                                             item$algorithm,
                                             item$runtime,
                                             item$mark,
                                             item$elapsed_senconds)
                       dbSendQuery(db_object, sql_string)
                       })


            output_message("Creating progress database is done")

            dbDisconnect(db_object)

            return(TRUE)
        }
    } else {
        # TODO
    }
}

#' query_progress_from_database
#' @param workshop_dir Directory of workshop
query_progress_from_database <- function(workshop_dir) {
    db_object <- connect_database(workshop_dir)

    sql_string <- "SELECT * FROM engine_progress"

    query_result <- dbSendQuery(db_object, sql_string)
    db_result <- dbFetch(query_result, n=-1)
    dbClearResult(query_result)
    dbDisconnect(db_object)

    return(db_result)
}

#' Main entry
#' @param workshop_dir Directory of workshop
#' @param configure Configure object
SDMengine <- function(workshop_dir, configure) {
    # TODO: using dplyr to make all thing faster, especily split operate
    # But dplyr can not install in old R platform

    make_progress_database(workshop_dir, configure)

    db_result <- query_progress_from_database(workshop_dir)

    parameter_data <- db_result

    parameter_list <- split(parameter_data, rownames(parameter_data))

    # setup default parallel setting
    parallel_core_number <- get_cpu_cores(workshop_dir)

    log_message <- sprintf("Using %s cores", parallel_core_number)
    output_message(log_message)

    mclapply(X=parameter_list,
             FUN=signal_worker,
             mc.cores=parallel_core_number,
             workshop_dir=workshop_dir,
             configure=configure)

    # For debug only
    #lapply(X=parameter_list,
    #         FUN=signal_worker,
    #         workshop_dir=workshop_dir,
    #         configure=configure)

    return(NULL)
}
