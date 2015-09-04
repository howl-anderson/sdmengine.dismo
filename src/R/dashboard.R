#' Dashboard
#' @param workshop_dir Directory of workshop
#' @export
dashboard <- function(workshop_dir) {
    progress_data <- query_progress_from_database(workshop_dir)
    valid_progress_data <- progress_data[progress_data[, "elapsed_time"] != -1, ]
    total_elapsed_senconds <- sum(valid_progress_data[, "elapsed_time"])

    # setup some default parallel setting
    # default number of CPU cores, all CPU cores - 1
    kDefaultCoreNumber <- detectCores() - 1
    parallel_core_number <- getOption("mc.cores", kDefaultCoreNumber)

    finished_task_number <- nrow(valid_progress_data)
    total_task_number <- nrow(progress_data)
    unfinished_task_number <- total_task_number - finished_task_number
    expected_time_to_finish_task <- unfinished_task_number / finished_task_number * total_elapsed_senconds

    result_data <- list(total_task_number=total_task_number,
                        finished_task_number=finished_task_number,
                        unfinished_task_number=unfinished_task_number,
                        total_elapsed_senconds=total_elapsed_senconds / parallel_core_number / 3600,
                        expected_time_to_finish_task=expected_time_to_finish_task / parallel_core_number / 3600)

    return(result_data)
}
