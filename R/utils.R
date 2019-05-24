
#NULL

vgsub <- function(pattern, replacement, x, ...) {
  for (i in 1:length(pattern)) x <- gsub(pattern[i], replacement[i], x, ...)
  x
  return(x)
}

trim_space <- function(x, what = c("both", "leading", "trailing", "none"), space.regex = "[:space:]", ...) {
  if (missing(x)) 
    stop("nothing to trim spaces to =(")
  re <- switch(match.arg(what), both = sprintf("^[%s]+|[%s]+$", space.regex, space.regex), 
               leading = sprintf("^[%s]+", space.regex), trailing = sprintf("[%s]+$", space.regex), 
               none = {
                 return(x)
               })
  FishSET:::vgsub(re, "", x, ...)
}

is_empty <- function(x, trim = TRUE, ...) {
  if (length(x) <= 1) {
    if (is.null(x)) 
      return(TRUE)
    if (length(x) == 0) 
      return(TRUE)
    if (is.na(x) || is.nan(x)) 
      return(TRUE)
    if (is.character(x) && nchar(ifelse(trim, FishSET:::trim_space(x), x)) == 0) 
      return(TRUE)
    if (is.logical(x) && !isTRUE(x)) 
      return(TRUE)
    if (is.numeric(x) && x == 0) 
      return(TRUE)
    return(FALSE)
  } else sapply(x, FishSET:::is_empty, trim = trim, ...)
}

find_first <- function(y){
  g <- y[which(grepl('date', names(y), ignore.case=TRUE) == TRUE)]
  if(all(g=='')==TRUE||all(FishSET:::is_empty(g)==TRUE)==TRUE) {warning('All date variables are empty')}
  g2 <- FishSET:::date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2==min(g2, na.rm=TRUE))[1]]
}

find_last <- function(y){
  g <- y[which(grepl('date', names(y), ignore.case=TRUE) == TRUE)]
  if(all(g=='')==TRUE||all(FishSET:::is_empty(g)==TRUE)==TRUE) {warning('All date variables are empty')}
  g2 <- FishSET:::date_parser(as.vector(unlist(c(g))))
  names(g)[which(g2==max(g2, na.rm=TRUE))[1]]
}

accumarray <- function(subs, val, sz = NULL, func = sum, fillval = 0) {
  stopifnot(is.numeric(subs), is.numeric(val))
  subs <- floor(subs)
  val <- c(val)
  if (any(subs < 1)) 
    stop("Argument 'subs' must be a matrix of integer indices.")
  matrix_p <- TRUE
  if (is.vector(subs)) {
    subs <- as.matrix(subs)
    matrix_p <- FALSE
  }
  n <- nrow(subs)
  m <- ncol(subs)
  if (length(val) < n) 
    stop("Length of 'vals' must not be smaller than no. of rows of 'subs'.")
  dm <- apply(subs, 2, max)
  if (!is.null(sz)) {
    if (length(sz) != ncol(subs) || any(sz < dm)) 
      stop("Argument 'sz' does not fit with 'subs'.")
    dm <- sz
  }
  if (m == 1) {
    A <- rep(fillval, dm)
    for (i in unique(subs)) {
      A[i] <- func(val[subs == i], na.rm = T)
    }
    if (matrix_p) 
      A <- as.matrix(A)
  } else {
    cm <- cumprod(dm[1:(m - 1)])
    A <- array(fillval, dim = dm)
    K <- numeric(n)
    for (i in 1:n) {
      K[i] <- subs[i, 1] + sum(cm * (subs[i, 2:m] - 1))
    }
    for (i in unique(K)) {
      A[i] <- func(val[K == i], na.rm = T)
    }
  }
  return(A)
}

skewness <- function(x) {
  n <- length(x)
  v <- var(x)
  m <- mean(x)
  third.moment <- (1/(n - 2)) * sum((x - m)^3)
  third.moment/(var(x)^(3/2))
}

date_parser <- function(dates){
  dates <- trimws(dates)
  dates <- sub(' .*','\\1',dates)
  if(!all(is.na(suppressWarnings(lubridate::mdy(dates)))==T)) {
    lubridate::mdy(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::dmy(dates)))==T)) {
    lubridate::dmy(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::ymd(dates)))==T)) {
    lubridate::ymd(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::ydm(dates)))==T)) {
    lubridate::ydm(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::myd(dates)))==T)) {
    lubridate::myd(dates)
  } else if(!all(is.na(suppressWarnings(lubridate::dym(dates)))==T)) {
    lubridate::dym(dates)
  } else {
    stop('Date format not recognized. Format date before proceeding')
  }
}

