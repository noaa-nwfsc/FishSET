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
#' @param output Whether to output `dat` with the expected catch variable added 
#'   (`'dataset'`) or to save an expected catch matrix to the expected catch 
#'   FishSET DB table (`'matrix'`). Defaults to `output = 'matrix'`. 
#' @md
#' @export
#' @importFrom stats lm
#' @importFrom rlang expr parse_expr sym
#' @importFrom dplyr select all_of expand_grid

catch_lm <- function(dat, project, catch.formula, zoneID = NULL, exp.name = NULL, 
                     new.name = NULL, output = 'matrix') {

  # call in dataset ----
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
   
  if (!is.null(exp.name)) {
    
    # get expected catch matrices (will throw error if ec list doesn't exist)
    ecl <- expected_catch_list(project)
    
    # if (is.null(new.name)) new.name <- exp.name
    # TODO: zoneID required
    for (i in seq_along(exp.name)) {
      
      # add each ec matrix as a column to dat
      dataset <- merge_expected_catch(dataset, project, zoneID, 
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
    
    if (!is.null(exp.name)) {
      
      # list of referenced ec matrices
      catch_mat <- ecl[exp.name]
      nz <- ncol(catch_mat[[1]])
      
      # create a list where each entry is a data.frame containing a single zone from 
      # each chosen ec matrix, e.g. list 1 = zone 1 from exp matrices 1 and 2,  etc.
      ec_cols <- lapply(seq_len(nz), function(i) {
        
        ec_i <- lapply(seq_along(exp.name), function(j) catch_mat[[j]][, i])
        names(ec_i) <- exp.name 
        as.data.frame(ec_i)
      })
      
      # use catch model to estimate expected catch for each zone
      ec_pred <- lapply(seq_len(nz), function(j) {
        
        # original data w/ response and ec variable removed
        dat.x <- dataset[cv[!cv %in% new.name & cv != cv[1]]]
        # add ec variables from ec_cols
        dat.x <- cbind(dat.x, ec_cols[[j]])
        
        predict(catch_mod, newdata = dat.x)
      })
      
      names(ec_pred) <- colnames(catch_mat[[1]])
      ec_matrix <- as.matrix(as.data.frame(ec_pred))
      
    } else { # use zoneID instead of ec matrix 
      
      z_sym <- rlang::sym(zoneID)
      dd <- 
        dat %>% 
        dplyr::select(-dplyr::all_of(zoneID)) %>% 
        dplyr::expand_grid(zone.temp = unique(dat[[zoneID]]))
      
      names(dd)[names(dd) == 'zone.temp'] <- zoneID
      
      pred_dd <- predict(catch_mod, newdata = dd)
      ec_matrix <- matrix(pred_dd, ncol = length(unique(dat[[zoneID]])), byrow = TRUE)
      dimnames(ec_matrix) <- list(NULL, unique(dat[[zoneID]]))
    }
    
    # TODO: assign row names using day
    
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
                                 new.name, output)
  catch_lm_function$kwargs <- list()
  
  log_call(project, catch_lm_function)
  
  if (output == 'dataset') dataset
  
}
