shift_sort_x <- function(x, ch, y, distance, alts, ab) {
    #' shift_sort_x
    #'
    #' Shifts choices so that the chosen zone will be automatically the first
    #'     one
    #'
    #' @param x Matrix of choice possibilites from create_logit_input
    #' @param ch Data corresponding to actual zonal choice
    #' @param y Data corresponding to actual catch
    #' @param distance Data corresponding to distance
    #' @param alts Number of alternative choices in model
    #' @param ab Number of cost parameters + number of alts
    #' @return d: matrix of choice possibilites and distance
    #' @export
    #' @examples 
    #'
    
    ch0 <- ch - 1
    n <- max(dim(ch))
    d <- list()
    x <- as.matrix(cbind(x, distance))
    # starts as data.frame
    ch <- as.matrix(ch)
    y <- as.matrix(y)
    
    for (j in 1:n) {
        if (ch0[j, ] == 0) {
            
            xsorted <- t(as.matrix(x[j, ]))
            # need to 'as.matrix' again because subsetting turns into named num
                # (one dim). Is there a not stupid way to do this?
            
        } else {
            
            xj <- as.matrix(x[j, ])
            #need to 'as.matrix' again because subsetting turns into named num
                #(one dim). Is there a not stupid way to do this?
            xj <- (matrix(xj, alts, ab))
            xj <- t(xj)
            
            xsorted <- cbind(xj[, ch[j, ]:dim(xj)[2]], xj[, 1:(ch[j, ] - 1)])
            xsorted <- t(xsorted)
            xsorted <- matrix(xsorted, 1, alts * ab)
            
        }
        
        d[[j]] <- cbind(as.matrix(y[j, ]), as.matrix(ch[j, ]), xsorted)
        
    }
    
    d <- do.call("rbind", d)
    
    return(d)
    
}
