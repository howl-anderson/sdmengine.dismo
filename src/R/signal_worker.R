#!/usr/bin/env Rscript

build_result_dir <- function(workshop_dir,
                             species_name,
                             environment_set,
                             runtime_number,
                             algorithm) {

    map_store_dir <- file.path(workshop_dir,
                               "result",
                               species_name,
                               environment_set,
                               algorithm,
                               runtime_number)
    if (!file.exists(map_store_dir)) {
        dir.create(map_store_dir, recursive=TRUE)
    }

    # suitability store path
    suitability_map_store_dir <- file.path(map_store_dir, "suitability")
    if (!file.exists(suitability_map_store_dir)) {
        dir.create(suitability_map_store_dir)
    }

    # distribution store path
    distribution_map_store_dir <- file.path(map_store_dir, "distribution")
    if (!file.exists(distribution_map_store_dir)) {
        dir.create(distribution_map_store_dir)
    }

    path_list <- list("suitability"=suitability_map_store_dir,
                      "distribution"=distribution_map_store_dir)
    return(path_list)
}

write_map_to_file <- function(workshop_dir,
                              species_name,
                              environment_set,
                              runtimes,
                              algorithm,
                              predict_continue_map) {

    # get suitability & distribution map dir
    dir_list = build_result_dir(workshop_dir,
                                species_name,
                                environment_set,
                                runtimes,
                                algorithm)
    suitability_map_store_dir <- dir_list["suitability"]

    file.name <- file.path(suitability_map_store_dir, "map.bil")
    writeRaster(predict_continue_map,
                filename=file.name,
                format="EHdr",
                overwrite=TRUE)

    return(NULL)
}

write_progress_to_database <- function(workshop_dir,
                                       species_name,
                                       runtimes,
                                       algorithm,
                                       start_time_seconds) {
    db_object <- connect_database(workshop_dir)

    end_time_seconds <- as.numeric(Sys.time())

    elapsed_seconds <- end_time_seconds - start_time_seconds

    sql_string = paste("UPDATE engine_progress SET mark=1, elapsed_time=",
                       elapsed_seconds, " WHERE species='",
                       species_name, "' AND algorithm='", algorithm,
                       "' AND runtime=", runtimes, sep="")

    results <- dbSendQuery(db_object, sql_string)

    output_message(sql_string)
    dbDisconnect(db_object)

    return(NULL)
}


output_result <- function(workshop_dir,
                          species_name,
                          runtimes,
                          algorithm,
                          predict_continue_map,
                          start_time_seconds,
                          other_continue_map_list) {
    # write base scene map
    base_environment_set <- "base"
    write_map_to_file(workshop_dir,
                      species_name,
                      base_environment_set,
                      runtimes,
                      algorithm,
                      predict_continue_map)

    for (predict_env_scene_name in names(other_continue_map_list)) {
        predict_raster <- other_continue_map_list[[predict_env_scene_name]]
        write_map_to_file(workshop_dir,
                          species_name,
                          predict_env_scene_name,
                          runtimes,
                          algorithm,
                          predict_raster)
    }

    write_progress_to_database(workshop_dir,
                               species_name,
                               runtimes,
                               algorithm,
                               start_time_seconds)

    return(NULL)
}

create_temp_dir <- function(species_name,
                            algorithm,
                            runtimes) {

    base_temp_dir <- file.path(tmpDir(), "sdmengine_dismo")
    # TODO: species_name need better clean
    species_name <- gsub(" ", "_", species_name)

    unique_dir <- paste(species_name,
                        algorithm,
                        runtimes,
                        as.integer(Sys.time()),
                        sep="_")
    temp_dir <- file.path(base_temp_dir, unique_dir)

    dir.create(path=temp_dir,
               showWarnings=FALSE,
               recursive=TRUE)

    return(temp_dir)
}

#' One CPU worker
#' @param parameter parameter of run
#' @param workshop_dir Directory of workshop
#' @param configure Configure object
#' @return NULL
signal_worker <- function(parameter, workshop_dir, configure) {
    parameter <- as.list(parameter)

    runtimes <- parameter[["runtime"]]
    species_name <- parameter[["species"]]
    algorithm <- parameter[["algorithm"]]
    task_flag <- parameter[["mark"]]

    predict_environment_set_list <- configure[["predict"]]
    environment_layer <- configure[["environment_layer"]]
    bg_point_num <- configure[["backgroud_point"]]
    presence_point_num <- configure[["presence_point"]]

    # check progress database if there already done
    if (task_flag) {
        # job is done already, just return.
        message_string <- sprintf("species='%s' and algorithms='%s' and runtime=%s was skipped, because it's done before",
                                  species_name,
                                  algorithm,
                                  runtimes)
        output_message(message_string)

        return(NULL)
    }

    start_time_seconds <- as.numeric(Sys.time())

    # load species specific setting
    species_configure <- load_species_setting(workshop_dir, species_name)

    # setup background point number
    # backgroud sample is a big problem
    kBgPointNum  <- 500 # default number of background points
    if (is.null(species_configure$background)) {
        if (is.null(bg_point_num)) {
            bg_point_num <- kBgPointNum
        }
    } else {
        bg_point_num <- species_configure$background
    }

    # setup presence point number that for sample
    # TODO: sample is a big problem
    # default number of presence points number: NULL present all points
    kPresencePointNum <- NULL
    if (is.null(species_configure$presence)) {
        if (is.null(presence_point_num)) {
            presence_point_num <- kPresencePointNum
        }
    } else {
        presence_point_num <- species_configure$presence
    }

    # read environment into raster stack
    base_environment_file_list <- paste0(file.path(workshop_dir,
                                                  "base_environment",
                                                  environment_layer),
                                        ".bil")
    base_environment_raster_stack <- stack(base_environment_file_list)

    # species dir
    species_path <- file.path(workshop_dir, "species", species_name)
    # raster file
    species_presence_raster_file <- file.path(species_path, "presence.bil")

    # make raster layer
    species_presence_layer <- raster(species_presence_raster_file)

    # absence raster file
    species_absence_raster_file <- file.path(species_path, "absence.bil")
    # make raster layer
    species_absence_layer <- raster(species_absence_raster_file)

    if (! is.null(presence_point_num)) {
        species_presence_point <- randomPoints(species_presence_layer,
                                               presence_point_num)
    } else {
        species_presence_point <-rasterToPoints(species_presence_layer)[, 1:2]
    }

    species_absence_point <- randomPoints(species_absence_layer, bg_point_num)

    # presence point env var
    presence_point_value <- extract(base_environment_raster_stack,
                                    species_presence_point)


    # work on predict environment set
    predict_environment_raster_list <- list()
    for (predict_environment_set in predict_environment_set_list) {
        predict_environment_set_path <- file.path(workshop_dir,
                                                  "environment",
                                                  predict_environment_set)

        environment_layer_file_list <- file.path(predict_environment_set_path,
                                                 environment_layer)
        environment_layer_file_list <- paste(environment_layer_file_list,
                                             ".bil",
                                             sep="")

        # read environment into raster stack
        predict_environment_stack <- stack(environment_layer_file_list)
        predict_environment_raster_list[[predict_environment_set]] <- predict_environment_stack
    }

    absence_point_value <- extract(base_environment_raster_stack,
                                   species_absence_point) # absence point env var
    flag_data <- c(rep(1, nrow(presence_point_value)),
                   rep(0, nrow(absence_point_value)))
    sdmdata <- data.frame(cbind(flag_data, rbind(presence_point_value,
                                                 absence_point_value))) # SDM data
    log_message <- sprintf("species='%s' runtime='%s' is under processing",
                           species_name,
                           runtimes)
    output_message(log_message)

    presence_train_point <- species_presence_point

    background_train_point <- species_absence_point

    train_point <- rbind(presence_train_point, background_train_point)
    train_flag_data <- c(rep(1, nrow(presence_train_point)),
                         rep(0, nrow(background_train_point)))
    train_value <- extract(base_environment_raster_stack, train_point)
    train_value <- data.frame(cbind(species=train_flag_data,
                                    train_value))

    temp_dir <- create_temp_dir(species_name,
                                algorithm,
                                runtimes)

    log_message <- sprintf("%s start",
                           algorithm)

    output_message(log_message)

    if ("bioclim" %in% algorithm) {
        model_object <- bioclim(base_environment_raster_stack,
                                presence_train_point)
    }
    if ("glml" %in% algorithm) {
        model_object <- glm(species ~ .,
                            family=binomial(link="logit"),
                            data=train_value)
    }
    if ("glmi" %in% algorithm) {
        model_object <- glm(species ~ .,
                            family=gaussian(link="identity"),
                            data=train_value)
    }
    if ("maxent" %in% algorithm) {

        maxent_temp_dir <- file.path(temp_dir, "maxent")
        dir.create(path=maxent_temp_dir,
                   showWarnings=FALSE,
                   recursive=TRUE)

        model_object <- maxent(base_environment_raster_stack,
                               presence_train_point,
                               path=maxent_temp_dir)
    }
    if ("rf" %in% algorithm) {
        model_object <- randomForest(species ~ .,
                                     data=train_value,
                                     na.action=na.omit)
    }
    if ("svm" %in% algorithm) {
        model_object <- ksvm(species ~ ., data=train_value)
    }

    predict_continue_map <- predict(base_environment_raster_stack, model_object)

    other_predict_raster_list <- list()

    for (predict_environment_set in names(predict_environment_raster_list)) {
        predict_environment_stack <- predict_environment_raster_list[[predict_environment_set]]

        other_continue_map <- predict(predict_environment_stack, model_object)
        other_predict_raster_list[[predict_environment_set]] <- other_continue_map
    }

    output_result(workshop_dir,
                  species_name,
                  runtimes,
                  algorithm,
                  predict_continue_map,
                  start_time_seconds,
                  other_predict_raster_list)

    log_message <- sprintf("%s end",
                           algorithm)

    output_message(log_message)

    # clean temp dir
    unlink(temp_dir, recursive=TRUE, force=TRUE)

    return(NULL)
}
