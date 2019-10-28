# discretefish_subroutine
#' Subroutine to run chosen discrete choice model
#'
#' @param project  Name of project. For obtaining catch, choice, distance, and otherdat data generated from make_model_design function. 
#' Working modelInputData table (table without date) will be pulled from fishset_db database.
#' @param initparams  Initial parameter estimates for revenue/location-specific covariates then cost/distance
#' @param optimOpt  Optimization options [max function evaluations, max iterations, (reltol) tolerance of x, trace]
# @param func Name of likelihood function
#' @param methodname Optimization method (see optim options)
#' @param mod.name Name of model run for model result output table
#' @param select.model Return an interactive data table that allows users to select and save table of best models based on measures of fit 
#' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
#' @export discretefish_subroutine
#' @importFrom DT DTOutput
#' @importFrom DBI dbExecute dbWriteTable dbExistsTable dbReadTable dbGetQuery dbDisconnect
#' @importFrom DT datatable JS DTOutput 
#' @importFrom stats optim
#' @import shiny
#' @return
#' \tabular{rlll}{
#' OutLogit: \tab [outmat1 se1 tEPM2] (coefs, ses, tstats) \cr 
#' optoutput: \tab optimization information \cr 
#' seoumat2: \tab ses \cr 
#' MCM: \tab Model Comparison metrics \cr 
#' }
#' @examples 
#choice <- as.data.frame(modelInputData$choice)
#alt.choice <- as.data.frame(as.numeric(as.factor(as.numeric(as.data.frame(modelInputData$choice)[,1]))))
#griddatfin <- list(predicted_catch=modelInputData$gridVaryingVariables$matrix)
#intdatfin <- list(modelInputData$bCHeader[[-1]])
#' \dontrun{
#' results <- discretefish_subroutine('pcod', initparams= c(0.5, -2.8), 
#'                                    optimOpt=c(100000,1.00000000000000e-08,1,1),
#'                                    func=logit_c, methodname="BFGS", 'newlogit4', 
#'                                    select.model=TRUE, project='projectName')
#' }




discretefish_subroutine <- function(project, initparams, optimOpt, methodname, mod.name, 
                                    select.model=FALSE,  name='discretefish_subroutine') {
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "modelinputdata LIMIT 1"))$ModelInputData[[1]])
  
  catch <- as.matrix(x[['catch']])
  choice <- x[['choice']]
  distance <- x[['distance']]
  startingloc <- x[['startingloc']]
  #otherdat <- list(griddat=list(griddatfin=x[['gridVaryingVariables']][['matrix']]), intdat=list(x[['bCHeader']][[-1]]), pricedata=list(epmDefaultPrice))
  
  choice.table <- as.matrix(choice, as.numeric(factor(choice)))
  choice <- as.matrix(as.numeric(factor(choice)))
  ab <- max(choice) + 1  #no interactions in create_logit_input - interact distances in likelihood function instead
  
  errorExplain <- NULL
  OutLogit <- NULL
  optoutput <- NULL
  seoutmat2 <- NULL
  MCM <- NULL
  H1 <- NULL
  fr <- x[['likelihood']]#func  #e.g. logit_c
  
  if(fr=='logit_correction' & all(is.na(startingloc))){
    'Stop. Startingloc parameter is not specified. Rerun the create_alternative_choice function'
  }
  dataCompile <- create_logit_input(choice)
  
  d <- shift_sort_x(dataCompile, choice, catch, distance, max(choice), ab)
  
  starts2 <- initparams

  ### Data needs will vary by the likelihood function ###
  if(grepl('epm', FishSET:::find_original_name(fr))){
    otherdat <- list(griddat=list(griddatfin=x[['bCHeader']][['gridVariablesInclude']]), intdat=list(x[['bCHeader']][['indeVarsForModel']]), pricedat=x[['epmDefaultPrice']])
    nexpcatch <- 1
    expname <-  FishSET:::find_original_name(fr)
  }  else if(FishSET:::find_original_name(fr)=='logit_correction'){
    otherdat <- list(griddat=list(griddatfin=data.frame(rep(1, nrow(choice)))),#x[['bCHeader']][['gridVariablesInclude']]), 
                     intdat=list(x[['bCHeader']][['indeVarsForModel']]),
                     startloc=x[['startloc']],
                     polyn=x[['polyn']])  
    nexpcatch <- 1
    expname <-  FishSET:::find_original_name(fr)
    } else if(FishSET:::find_original_name(fr)=='logit_avgcat'){
    otherdat <- list(griddat=list(griddatfin=data.frame(rep(1, nrow(choice)))),#x[['bCHeader']][['gridVariablesInclude']]), 
                                  intdat=list(x[['bCHeader']][['indeVarsForModel']]))  
    nexpcatch <- 1
    expname <-  FishSET:::find_original_name(fr)
  } else if(FishSET:::find_original_name(fr)=='logit_c'){
    nexpcatch <- length(names(x[['gridVaryingVariables']]))-2
  }
  #Begin loop  
  for(i in 1:nexpcatch){
    if(FishSET:::find_original_name(fr)=='logit_c'){
    expname <- paste0(names(x[['gridVaryingVariables']])[i],'_',FishSET:::find_original_name(fr))
    otherdat <- list(griddat=list(griddatfin=x[['gridVaryingVariables']][[names(x[['gridVaryingVariables']])[i]]]),intdat=list(x[['bCHeader']][['indeVarsForModel']]))
    }

  
  LL_start <- fr(starts2, d, otherdat, max(choice), project, expname, mod.name)
  
  if (is.null(LL_start) || is.nan(LL_start) || is.infinite(LL_start)) {
    # haven't checked what happens when error yet
    errorExplain <- "Initial function results bad (Nan, Inf, or undefined), check 'ldglobalcheck'"
    cat("Initial function results bad (Nan, Inf, or undefined), check 'ldglobalcheck'")
    next
  }
  
  ############################################################################# 
  mIter <- optimOpt[1] #should add something to default options here if not specified
  relTolX <- optimOpt[2]
  reportfreq <- optimOpt[3]
  detailreport <- optimOpt[4]
  
  controlin <- list(trace=detailreport,maxit=mIter,reltol=relTolX,REPORT=reportfreq)
  
  res <- tryCatch({
    
    stats::optim(starts2, fr, dat = d, otherdat = otherdat, alts = max(choice), method = methodname, 
                 control = controlin, hessian = TRUE, project=project, expname=expname, mod.name=mod.name)
    
  }, error = function(e) {
    
    return("Optimization error, check 'ldglobalcheck'")
    
  })
  
  if (res[[1]][1] == "Optimization error, check 'ldglobalcheck'") {
    
    print(list(name=names(x[['gridVaryingVariables']])[i], errorExplain = res, OutLogit = OutLogit, optoutput = optoutput, 
                seoutmat2 = seoutmat2, MCM = MCM, H1 = H1))
    next
  }
  
  q2 <- res[["par"]]
  LL <- res[["value"]]
  output <- list(counts = res[["counts"]], convergence = res[["convergence"]], 
                 optim_message = res[["message"]])
  H <- res[["hessian"]]
  #############################################################################  
  # Model comparison metrics (MCM)
  param <- max(dim(as.matrix(starts2)))
  obs <- dim(dataCompile)[1]
  AIC <- round(2 * param - 2 * LL,3)
  
  AICc <- round(AIC + (2 * param * (param + 1))/(obs - param - 1),3)
  
  BIC <- round(-2 * LL + param * log(obs),3)
  
  PseudoR2 <- round((LL_start - LL)/LL_start,3)
  if (!exists("mod.out")) {
    mod.out <- data.frame(matrix(NA, nrow = 4, ncol = 1))
    mod.out[, 1] = c(AIC, AICc, BIC, PseudoR2)
    rownames(mod.out) = c("AIC", "AICc", "BIC", "PseudoR2")
    colnames(mod.out) = paste0(expname,mod.name)
  } else {
    temp <- data.frame(c(AIC, AICc, BIC, PseudoR2))
    colnames(temp) = paste0(expname,mod.name)
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  
  if (DBI::dbExistsTable(fishset_db, paste0(project,"modelfit")) == FALSE) {
    DBI::dbWriteTable(fishset_db, paste0(project,"modelfit"), mod.out )
  } else {
    out.mod <- DBI::dbReadTable(fishset_db, paste0(project, "modelfit"))
    if(exists('temp')){
      out.mod <- cbind(out.mod, temp)
    } else {
      out.mod <- cbind(out.mod, mod.out)
    }
    
    if (any(duplicated(colnames(out.mod))) == T) {
      warning("Duplicate columns names. Please define a unique column name for the model output.")
    }
    DBI::dbWriteTable(fishset_db, paste0(project, "modelfit"), out.mod, overwrite = T)
  }
  
  ### Full model output
  MCM <- list(AIC = AIC, AICc = AICc, BIC = BIC, PseudoR2 = PseudoR2)
  
  if (is.null(H) == FALSE) {
    
    Htrial <- function(x){
      Htrial = tryCatch({
      solve(H)
    }, error = function(e) {
      return("Error, singular, check 'ldglobalcheck'")      
    })
      Htrial
    }    
    print(Htrial(H))
    H1 <- Htrial(H)


    diagtrial <- function(x){
        diagtrial = tryCatch({
        diag(H1)
        }, error = function(e) {
          return("Error, NAs, check 'ldglobalcheck'")
          })
        diagtrial
      }
    if(H1[1]!="Error, singular, check 'ldglobalcheck'"){
      diag2 <- diagtrial(H1)
      print(diag2)
      }
    
    
    if(H1[1]!="Error, singular, check 'ldglobalcheck'"){
      if(diag2[1]!="Error, NAs, check 'ldglobalcheck'"){
    se2 <- tryCatch({
      sqrt(diag2)
    }, warning = function(war) {
      print("Check 'ldglobalcheck'")
      sqrt(diag2)
    })
    }}
    
    if(H1[1]!="Error, singular, check 'ldglobalcheck'"){
    outmat2 <- t(q2)
    seoutmat2 <- t(se2)
    optoutput <- output
    tLogit <- t(outmat2/se2)
    OutLogit <- cbind(t(outmat2), as.matrix(se2), (tLogit))
    }
  }
  
  #if(H1[1]!="Error, singular, check 'ldglobalcheck'") next
  
  if(exists('modelOut')) {
     modelOut[[length(modelOut)+1]] <- list(name=expname,errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
                     seoutmat2 = seoutmat2, MCM = MCM, H1 = H1, choice.table=choice.table)
    } else {
      modelOut <-  list()
      modelOut[[length(modelOut)+1]] <- list(name=expname,errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
                   seoutmat2 = seoutmat2, MCM = MCM, H1 = H1, choice.table=choice.table)
    }
  single_sql <- paste0(project, "modelOut", format(Sys.Date(), format="%Y%m%d"))
  if(table_exists(single_sql)){
    table_remove(single_sql)
  }
  second_sql <- paste("INSERT INTO", single_sql, "VALUES (:data)")
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(data modelOut)"))
  DBI::dbExecute(fishset_db, second_sql, params = list(data = list(serialize(modelOut, NULL))))
  DBI::dbDisconnect(fishset_db)
  }
  #### End looping through expectated catch cases
  
  #out.mod <<- out.mod
  ############################################################################# 
  if(select.model==TRUE){
    #  rownames(out.mod)=c("AIC", "AICc", "BIC", "PseudoR2")
    #   print(DT::datatable(t(round(out.mod, 5)), filter='top'))
    library(shiny)
    shiny::runApp(list(
      ui = shiny::basicPage(
        
        h2('Model Output'),
        DT::DTOutput("mytable"),
        h3(''),
        actionButton("submit", "Save table", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;display:inline-block;width:12%;text-align: center;"),
        tags$button(
          id = 'close',
          type = "button",
          style="color: #fff; background-color: #FF6347; border-color: #800000; display:inline-block;width:12%;text-align: center;margin-left:10px",
          class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},500);",  # close browser
          "Close window"
        )
      ),
      
      server = function(input, output) {
        # helper function for making checkbox
        shinyInput = function(FUN, len, id, ...) { 
          inputs = character(len) 
          for (i in seq_len(len)) { 
            inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
          } 
          inputs 
        } 
        # datatable with checkbox
        output$mytable = shiny::renderDataTable({
          data.frame(t(out.mod),Select=shinyInput(checkboxInput,nrow(t(out.mod)),"cbox_"))
        }, colnames=c('model','AIC','AICc','BIC','PseudoR2'), filter='top', server = FALSE, escape = FALSE, options = list( 
          paging=FALSE,
          preDrawCallback = DT::JS('function() { 
                                   Shiny.unbindAll(this.api().table().node()); }'), 
          drawCallback = DT::JS('function() { 
                                Shiny.bindAll(this.api().table().node()); } ') 
          ) )
        # helper function for reading checkbox
        
        shinyValue = function(id, len) { 
          unlist(lapply(seq_len(len), function(i) { 
            value = input[[paste0(id, i)]] 
            if (is.null(value)) NA else value 
          })) 
        } 
        
        shinyDate = function(id, len) { 
          unlist(lapply(seq_len(len), function(i) { 
            #value = input[[paste0(id, i)]]
            #if (is.null(value)) NA else value
            value=ifelse(input[[paste0(id, i)]]!=TRUE, '' , as.character(Sys.Date())) 
          })) 
        }
        
        checkedsave <- reactive(cbind(
          model = colnames(out.mod), t(out.mod), 
          selected = shinyValue("cbox_", nrow(t(out.mod))), 
          Date = shinyDate("cbox_", nrow(t(out.mod)))))
        
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
          # Connect to the database
          fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
          single_sql <- paste0(project, "modelChosen")
          if(DBI::dbExistsTable(fishset_db, single_sql)==FALSE){
            DBI::dbExecute(fishset_db, paste0("CREATE TABLE ",single_sql,"(model TEXT, AIC TEXT, AICc TEXT, BIC TEXT, PseudoR2 TEXT, selected TEXT, Date TEXT)"))
          }
          # Construct the update query by looping over the data fields
          query <- sprintf(
            "INSERT INTO %s (%s) VALUES %s",
            single_sql, 
            paste(names(data.frame(as.data.frame(isolate(checkedsave())))), collapse = ", "),
            paste0("('", matrix(apply(as.data.frame(isolate(checkedsave())), 1, paste, collapse="','"), ncol=1), collapse=',', "')")
          )
          # Submit the update query and disconnect
          DBI::dbGetQuery(fishset_db, query)
          showNotification("Table saved to database")
        })
        
        # stop shiny
        observe({
          if (input$close > 0) stopApp()                             
        })
        
        
  }
    ))
    
}
  
  ############################################################################# 
  discretefish_subroutine_function <- list()
  discretefish_subroutine_function$functionID <- 'discretefish_subroutine'
  discretefish_subroutine_function$args <- c(project, initparams, optimOpt, deparse(substitute(func)), methodname, mod.name)
  discretefish_subroutine_function$kwargs <- list()
  discretefish_subroutine_function$output <- c(name)
  log_call(discretefish_subroutine_function)
   ############################################################################# 
  single_sql <- paste0(project, "modelOut", format(Sys.Date(), format="%Y%m%d"))
  if(table_exists(single_sql)){
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", single_sql, " LIMIT 1"))$data[[1]])
  return(x)
  DBI::dbDisconnect(fishset_db)
  }
  }
