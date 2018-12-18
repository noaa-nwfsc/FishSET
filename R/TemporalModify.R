#'  Transform units of a temporal variables 
#'
#' 

#' @param dataset dataframe or matrix
#' @param x Time variable to modify 
#' @keywords Date, as.Date
#' @return Returns variable with modified time unites
#' @details define.format defines the format that the variable should take on. Examples include '%Y%m%d', '%Y-%m-%d %H:%M:%S'.
#' The function also has options to extract year, month, minute, or month/day. These are defined through fun.mod
#' 
#' @examples 
#' Date_Landed_YMD <- TempMod(MainDataTable, 'DATE_LANDED', fun.mod='',define.format = '%Y%m%d')
#' Date_Landed_year <- TempMod(MainDataTable, 'DATE_LANDED', fun.mod='year',define.format = '')


# Change to Year, Month, day,
TempMod <- function(dataset, x, fun.mod = "", define.format) {
    if (fun.mod == "") {
        # User defines the format of the time variable
        int <- format(dataset[[x]], format = define.format)
        # Extract specific time unit
    } else {
        if (fun.mod == "month") {
            # Month:
            ref <- format(dataset[[x]], format = "%Y-%m-%d")
            int <- months(as.Date(ref))
        } else if (fun.mod == "year") {
            # Year:
            int <- as.numeric(format(dataset[[x]], format = "%Y"))
        } else if (fun.mod == "month/day") {
            # Month/day
            int <- format(dataset[[x]], format = "%m/%d")
        } else {
            warning("fun.mod is not recognized. Choices include, month, year,month/day, minute")
        }
    }
    # logging function information
    df.name <- deparse(substitute(dataset))
    write(layout.json.ed(trace, "TempMod", df.name, x, msg = paste("fun.mod:", fun.mod, "; define.format:", define.format)), paste(getwd(), "/Logs/", Sys.Date(), 
        ".json", sep = ""), append = T)
    
}
