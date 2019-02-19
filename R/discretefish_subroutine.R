#' discretefish_subroutine
#' Subroutine to run chosen discrete choice model
#'
#' @param catch Data corresponding to actual zonal choice
#' @param choice Data corresponding to actual catch
#' @param distance Data corresponding to distance
#' @param otherdat Other data (as a list)
#' @param initparams Initial parameter estimates for revenue/location-specific covariates then cost/distance
#' @param optimOpt Optimization options [max function evaluations, max iterations, (reltol) tolerance of x]
#' @param func Name of likelihood function
#' @param methodname Optimization method (see optim options)
#' @param func.name Name of likelihood function for model result output table
#' @param select.model Return an interactive data table of model output that allows users to select and save table of best models
#' @importFrom DBI dbExecute dbWriteTable dbExistsTable dbReadTable dbGetQuery dbDisconnect
#' @importFrom DT datatable JS
#' @import shiny
#' @return
#' OutLogit - [outmat1 se1 tEPM2] (coefs, ses, tstats) \cr 
#' optoutput - optimization information \cr 
#' seoumat2 - ses \cr 
#' MCM - Model Comparison metrics \cr 
#' H1 - inverse hessian \cr 
#' 
# @examples 
#choice <- as.data.frame(modelInputData$choice)
#alt.choice <- as.data.frame(as.numeric(as.factor(as.numeric(as.data.frame(modelInputData$choice)[,1]))))
#griddatfin <- list(predicted_catch=modelInputData$gridVaryingVariables$matrix)
#intdatfin <- list(modelInputData$bCHeader[[-1]])
#results <- discretefish_subroutine(catch=as.data.frame(modelInputData$catch), alt.choice, distance=modelInputData$zonalChoices,
#                                   otherdat=list(griddat=griddatfin,intdat=intdatfin), initparams= c(0.5, -2.8), optimOpt=c(100000,1000000,1.00000000000000e-06),
#                                   func=logit_c, methodname="BFGS", 'newlogit4', select.model=TRUE)




<<<<<<< R/discretefish_subroutine.R
discretefish_subroutine <- function(catch, choice, distance, otherdat, initparams, optimOpt, func, methodname, func.name, return.table=FALSE) {
=======
discretefish_subroutine <- function(catch, choice, distance, otherdat, initparams, optimOpt, func, func.name, select.model=FALSE) {
>>>>>>> R/discretefish_subroutine.R
  
  errorExplain <- NULL
  OutLogit <- NULL
  optoutput <- NULL
  seoutmat2 <- NULL
  MCM <- NULL
  H1 <- NULL
  fr <- func  #e.g. clogit
  
  ab <- max(choice) + 1  #no interactions in create_logit_input - interact distances in likelihood function instead
  dataCompile <- create_logit_input(choice)
  
  d <- shift_sort_x(dataCompile, choice, catch, distance, max(choice), ab)
  
  starts2 <- initparams
  
  LL_start <- fr(starts2, d, otherdat, max(choice))
  
  if (is.null(LL_start) || is.nan(LL_start) || is.infinite(LL_start)) {
    # haven't checked what happens when error yet
    errorExplain <- "Initial function results bad (Nan, Inf, or undefined), check 'ldglobalcheck'"
    return("Initial function results bad (Nan, Inf, or undefined), check 'ldglobalcheck'")
  }
  
  ############################################################################# 
  mIter <- optimOpt[1] #should add something to default options here if not specified
  relTolX <- optimOpt[2]
  reportfreq <- optimOpt[3]
  detailreport <- optimOpt[4]
  
  controlin <- list(trace=detailreport,maxit=mIter,reltol=relTolX,REPORT=reportfreq)
  
  res <- tryCatch({
    
    optim(starts2, fr, dat = d, otherdat = otherdat, alts = max(choice), method = methodname, 
			control = controlin, hessian = TRUE)
    
  }, error = function(e) {
    
    return("Optimization error, check 'ldglobalcheck'")
    
  })
  
  if (res[[1]][1] == "Optimization error, check 'ldglobalcheck'") {
    
    return(list(errorExplain = res, OutLogit = OutLogit, optoutput = optoutput, 
                seoutmat2 = seoutmat2, MCM = MCM, H1 = H1))
    
  }
  
  q2 <- res[["par"]]
  LL <- res[["value"]]
  output <- list(counts = res[["counts"]], convergence = res[["convergence"]], 
                 optim_message = res[["message"]])
  H <- res[["hessian"]]
  
  # Model comparison metrics (MCM)
  param <- max(dim(as.matrix(starts2)))
  obs <- dim(dataCompile)[1]
  AIC <- 2 * param - 2 * LL
  
  AICc <- AIC + (2 * param * (param + 1))/(obs - param - 1)
  
  BIC <- -2 * LL + param * log(obs)
  
  PseudoR2 <- (LL_start - LL)/LL_start
  
  if (!exists("mod.out")) {
    mod.out <- data.frame(matrix(NA, nrow = 4, ncol = 1))
    mod.out[, 1] = c(AIC, AICc, BIC, PseudoR2)
    rownames(mod.out) = c("AIC", "AICc", "BIC", "PseudoR2")
    colnames(mod.out) = func.name
  } else {
    temp <- data.frame(c(AIC, AICc, BIC, PseudoR2))
    colnames(temp) = func.name
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  
  if (DBI::dbExistsTable(fishset_db, "out.mod") == FALSE) {
    DBI::dbWriteTable(fishset_db, "out.mod", out)
  } else {
    out.mod <- DBI::dbReadTable(fishset_db, "out.mod")
    if(exists('temp')){
    out.mod <- cbind(out.mod, temp)
    } else {
    out.mod <- cbind(out.mod, mod.out)
    }
    
    if (any(duplicated(colnames(out.mod))) == T) {
      stop("Duplicate columns names. Please define a unique column name for the model output,")
    }
    DBI::dbWriteTable(fishset_db, "out.mod", out.mod, overwrite = T)
  }

    out.mod <<- out.mod

 if(select.model==TRUE){
 #  rownames(out.mod)=c("AIC", "AICc", "BIC", "PseudoR2")
 #   print(DT::datatable(t(round(out.mod, 5)), filter='top'))
    
    runApp(list(
      ui = basicPage(
        
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
        output$mytable = DT::renderDataTable({
          data.frame(t(out.mod),Select=shinyInput(checkboxInput,nrow(t(out.mod)),"cbox_"))
        }, filter='top', server = FALSE, escape = FALSE, options = list( 
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
          
          if(DBI::dbExistsTable(fishset_db, 'modelChosen')==FALSE){
            DBI::dbExecute(fishset_db, "CREATE TABLE modelChosen(model TEXT, AIC TEXT, AICc TEXT, BIC TEXT, PseudoR2 TEXT, selected TEXT, Date TEXT)")
          }
          # Construct the update query by looping over the data fields
          query <- sprintf(
            "INSERT INTO %s (%s) VALUES %s",
            "modelChosen", 
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
  
  MCM <- list(AIC = AIC, AICc = AICc, BIC = BIC, PseudoR2 = PseudoR2)
  
  if (is.null(H) == FALSE) {
    
    H1 <- tryCatch({
      solve(H)
    }, error = function(e) {
      return("Error, singular, check 'ldglobalcheck'")      
    })
    
    diag2 <- tryCatch({
      diag(H1)
    }, error = function(e) {
      return("Error, NAs, check 'ldglobalcheck'")
    })
    
    se2 <- tryCatch({
      sqrt(diag2)
    }, warning = function(war) {
      print("Check 'ldglobalcheck'")
      sqrt(diag2)
    })
    
    outmat2 <- t(q2)
    seoutmat2 <- t(se2)
    optoutput <- output
    tLogit <- t(outmat2/se2)
    OutLogit <- cbind(t(outmat2), as.matrix(se2), (tLogit))
  }
  
  modelOut <- list(errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
                   seoutmat2 = seoutmat2, MCM = MCM, H1 = H1)

 
  DBI::dbExecute(fishset_db, "CREATE TABLE IF NOT EXISTS data (modelout modelOut)")
  DBI::dbExecute(fishset_db, "INSERT INTO data VALUES (:modelout)", params = list(modelout = list(serialize(modelOut, NULL))))
  DBI::dbDisconnect(fishset_db)
  
  
  write(layout.json.ed(trace, "discretefish_subroutine", dataset = "", x = "", 
                       msg = paste("catch:", deparse(substitute(catch)), ", choice:", deparse(substitute(choice)), 
                                   ", distance:", deparse(substitute(distance)), ", otherdat:", deparse(substitute(otherdat)), 
                                   ", initparams:", deparse(substitute(initparams)), ", optimOpt:", deparse(substitute(optimOpt)), 
                                   ", func:", deparse(substitute(func)), ", methodname:", deparse(substitute(methodname)))), 
        paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  ############################################################################# 
  
  return(list(errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
              seoutmat2 = seoutmat2, MCM = MCM, H1 = H1))
  
}
