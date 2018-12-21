#'  Create variable or matrix
#'
#' @param dataset dataframe or matrix
#' @param dataindex dataframe that contains information on each column of the dataset
#' @param ... Column(s) that define the variable(s) over which to make new ID
#' @param x Variable to create 
#' @param DumFill Fill the dummy variable with TRUE or FALSE

#' @return Returns the dataframe with new Dummy variable included
#' @details Function for generating specialized variables.
#' 
# @examples 
# DumVar <- DummyVar(MainDataTable)
# DumMat <- DummyMatrix(MainDataTable, 'DISEMBARKED_PORT')

##--- CPUE ----##
cpue <- function(dataset, xWeight, xTime) {
    # logging function information
    df.name <- deparse(substitute(dataset))
    x.weight <- deparse(substitute(xWeight))
    x.time <- deparse(substitute(xWeight))
    # write(layout.json.ed(trace, 'cpue', df.name, x=x.weight, msg=x.time), paste('~/FistSET_RPackage/Logs/Log_file', Sys.Date(), '.json'), append=T) Check
    # that Weight variable is indeed a weight variable
    if (grepl("LB|Pounds|MT", xWeight, ignore.case = TRUE)) {
        if (grepl("Min|Hour|Duration", xTime, ignore.case = TRUE)) {
            if (!grepl("Duration", xTime, ignore.case = TRUE)) {
                warning("xTime should be a length of time such as duration in minutes")
            }
            dataset[[xWeight]]/dataset[[xTime]]
        } else {
            stop("xTime must be a time measurement")
        }
    } else {
        stop("xWeight must be in units of pounds or metric tons.")
    }
}


##---- Dummy  Variables ----##
# create a new dummy variabler
DummyVar <- function(dataset, DumFill = TRUE) {
    as.vector(rep(DumFill, nrow(dataset)))
    # logging function information
    df.name <- deparse(substitute(dataset))
    write(layout.json.ed(trace, "DummyVar", df.name, x = ""), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
}


# Create dummy matrix from a coded ID variable
DummyMatrix <- function(dataset, x) {
    df.name <- deparse(substitute(dataset))
    x.name <- deparse(substitute(x))
    write(layout.json.ed(trace, "DummyMatrix", df.name, x = x.name), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
    # create the matrix
    factor.levels <- levels(as.factor(dataset[[x]]))
    int <- data.frame(matrix(rep(dataset[[x]], length(factor.levels)), ncol = length(factor.levels)))
    colnames(int) = factor.levels
    # change matrix to TRUE/FALSE
    int <- data.frame(lapply(1:length(factor.levels), function(x) ifelse(int[, x] == colnames(int)[x], TRUE, FALSE)))
    colnames(int) = paste(x, "_", levels(as.factor(dataset[[x]])))
    return(int)
}



##---- Numeric  Variables ----##


##---- Spatial  Variables ----##


##---- Temporal  Variables ----##

