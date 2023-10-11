#' Linear Model for Catch
#' 
#' First stage regression model for catch. 
#' 
#' @param dat Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param catch.formula A formula object specifying the linear model.See [stats::lm()].
#' @param zoneID zone ID Variable in `dat` that identifies the individual zones or 
#'   areas. Required if merging expected catch into `dat` using `exp.name`, or 
#'   when creating a new expected catch matrix  and `exp.name` is `NULL` (see 
#'   `output` below).
#' @param exp.name Name(s) of expected catch matrix to merge into `dat`.
#' @param new.name Optional, string. When `output = 'matrix'`, `new.name` will 
#'   become the name of the new expected catch matrix saved to the FishSET DB
#'   expected catch list. When `output = 'dataset'`, `new.name` will 
#'   become the name of the new expected catch variable added to the primary
#'   dataset. 
#' @param date Date variable from `dat` used to create expected catch matrix. 
#' @param output Whether to output `dat` with the expected catch variable added 
#'   (`'dataset'`) or to save an expected catch matrix to the expected catch 
#'   FishSET DB table (`'matrix'`). Defaults to `output = 'matrix'`. 
#' @details `catch_lm()` can merge an expected catch matrix into the primary dataset
#'   before running the linear model. This is done using by passing `exp.name` 
#'   and `zoneID` to [merge_expected_catch()] and is for convenience; users can 
#'   do this separately using [merge_expected_catch()] if desired, just make sure 
#'   to leave `exp.name` empty before running `catch_lm()`. Merging expected catch
#'   in a separate step is useful for creating tables and plots before running 
#'   a first stage linear regression. 
#' @return `catch_lm()` has two output options: `dataset` and `matrix`. When 
#'   `output == 'dataset'`, the primary dataset will be returned with the fitted
#'   values from the model added as a new column. The new column is named using 
#'   `new.name`. 
#'   
#'   When `output == 'matrix'` an expected catch matrix is created and saved to 
#'   the FishSET DB expected catch list (it is not outputted to the console). 
#'   There are two ways to create an expected catch matrix: by using an existing 
#'   expected catch matrix in `catch.formula`, or by using a zone-identifier
#'   column (i.e. `zoneID`) in the `catch.formula`. For example, if you have 
#'   created an expected catch matrix named 'user1' using [create_expectations()],
#'   `catch.formula` could equal `catch ~ vessel_length * user1`. In this case
#'   `exp.name` would equal `'user1'`. Alternatively, you could create an expected 
#'   catch matrix by specifying `catch.formula` as `catch ~ vessel_length * zone`.
#'   In this case, `exp.name = NULL` and `zoneID = 'zone'`. 
#' @seealso [merge_expected_catch()]
#' @md
#' @export
#' @importFrom stats lm predict
#' @importFrom rlang expr
#' @importFrom dplyr select all_of 
#' @importFrom tidyr expand_grid pivot_wider

catch_lm <- function(dat, 
                     project, 
                     catch.formula, 
                     zoneID = NULL, 
                     exp.name = NULL, 
                     new.name = NULL, 
                     date, 
                     output = 'matrix') {

  # call in dataset ----
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  column_check(dataset, c(zoneID, date))
   
  if (!is.null(exp.name)) {
    
    # get expected catch matrices (will throw error if ec list doesn't exist)
    ecl <- expected_catch_list(project)
    
    for (i in seq_along(exp.name)) {
      
      # add each ec matrix as a column to dat
      dataset <- merge_expected_catch(dataset,
                                      project, 
                                      zoneID, 
                                      date = date,
                                      exp.name = exp.name[i], 
                                      new.name = NULL, 
                                      log_fun = FALSE)
    }
  }
  
  # specify linear model ----
  # TODO: save lm output
    
  # lm_dat <- rlang::parse_expr(dataset)
  # lm_call <- rlang::expr(stats::lm(!!catch.formula, data = !!lm_dat)) # include ... for added options?
  lm_call <- rlang::expr(stats::lm(!!catch.formula, data = dataset))
  
  catch_mod <- eval(lm_call)
  
  print(summary(catch_mod))
  
  cv <- all.vars(catch.formula)
  
  # output matrix ----
  if (output == 'matrix') {
    
    no_zone <- is.null(zoneID) || !zoneID %in% cv
    
    if (is.null(exp.name) & no_zone) {
      
      stop('An expected catch matrix must be used ("exp.name") or a zone/area ', 
           'term must be used in linear model ("zoneID") for output = "matrix"', 
           call. = FALSE)
    }
    
    # output matrix ----
    if (!is.null(exp.name)) {
      
      # list of referenced ec matrices
      catch_mat <- ecl[exp.name]
      nz <- ncol(catch_mat[[1]])     # number of zones
      zn <- colnames(catch_mat[[1]]) # zone names
      
      # create a list where each entry is a data.frame containing a single zone from 
      # each chosen ec matrix, e.g. list 1 = zone 1 from exp matrices 1 and 2,  etc.
      ec_cols <- lapply(seq_len(nz), function(i) {
        # i = expected catch matrix i
        # j = zone j
        ec_i <- lapply(seq_along(exp.name), function(j) catch_mat[[j]][, i])
        names(ec_i) <- exp.name 
        as.data.frame(ec_i)
      })
      
      # use catch model to estimate expected catch for each zone
      ec_pred <- lapply(seq_len(nz), function(j) {
        # j = zone j
        # original data w/ response and ec variable removed
        dat.x <- dataset[cv[!cv %in% new.name & cv != cv[1] & cv != zoneID]]
        # add expected catch from column j
        dat.x <- cbind(dat.x, ec_cols[[j]])
        # add zone
        dat.x[[zoneID]] <- zn[j]
        # predict catch using zone j
        predict(catch_mod, newdata = dat.x)
      })
      
      # convert to matrix
      names(ec_pred) <- colnames(catch_mat[[1]])
      ec_matrix <- as.matrix(as.data.frame(ec_pred))
      row.names(ec_matrix) <- as.character(dataset[[date]])
      
    } else { # use zoneID instead of ec matrix 
      
      # add fitted values to dataset
      dataset[[new.name]] <- catch_mod$fitted.values
      
      # filter data to include date, zoneID, and fitted values
      dd <- 
        dataset %>% 
        select(all_of(c(date, zoneID, new.name))) %>% 
        # widen data by zone
        tidyr::pivot_wider(id_cols = !!date, 
                           names_from = all_of(zoneID), 
                           values_from = all_of(new.name), 
                           values_fill = 0, # fill missing values w/ 0
                           values_fn = mean) # when more than one value exists for date-zone, output the mean
      
      # arrange columns 
      dd <- dd %>% dplyr::select(all_of(sort(names(dd))))
      
      # convert to matrix
      ec_matrix <- as.matrix(dd[-1]) # drop date
      row.names(ec_matrix) <- as.character(dd[[date]]) # add date as row name
      
      # pull rows matched by day 
      ecList <- 
        lapply(dataset[[date]], function(p) {
          
          ec_matrix[p == row.names(ec_matrix), , drop = FALSE]
        })
      # combine list into a matrix, dim = n obs x n zones (fishset format)
      ec_matrix <- do.call(rbind, ecList)
    }
    
    # create a default name if new.name is NULL
    if (is.null(new.name)) {
      
      new.name <- 'catch_lm'
      # check if default name used already, if so increment by 1
      name_match <- grep('catch_lm\\d+$', names(ecl))
    
      if (length(name_match) > 0) new.name <- paste0(new.name, length(name_match) + 1)
      else new.name <- paste0(new.name, 1)
    }
   
    # add new matrix w/ new name
    ecl[[new.name]] <- ec_matrix
    
    # connect to FishSET DB
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    # save to FishSET DB
    ec_sql <- paste0(project, "ExpectedCatch")
    
    if (table_exists(ec_sql, project)) {
      
      table_remove(ec_sql, project)
    }
    
    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", ec_sql, 
                                     "(data ExpectedCatch)"))
    DBI::dbExecute(fishset_db, 
                   paste("INSERT INTO", ec_sql, "VALUES (:data)"),
                   params = list(data = list(serialize(ecl, NULL)))
    )
    
    message(paste0('"', new.name, '" added to expected catch list.'))
    
  } else {
    # output dataset ----
    dataset[[new.name]] <- catch_mod$fitted.values
  }
  
  # log function ----
  catch_lm_function <- list()
  catch_lm_function$functionID <- "catch_lm"
  catch_lm_function$args <- list(dat, project, catch.formula, zoneID, exp.name, 
                                 new.name, date, output)
  catch_lm_function$kwargs <- list()
  
  log_call(project, catch_lm_function)
  
  if (output == 'dataset') dataset
  
}
