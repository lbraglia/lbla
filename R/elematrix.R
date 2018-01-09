#' Elementary matrix generation
#'
#' Elementary matrix generation (Salce pag 46)
#' 
#' @param n dimnension of the square matrix
#' @param i row number
#' @param j column number
#' @param alpha a scalar
#' @export
elematrix <- function(n = stop("n must be specified"), 
                      i = stop("i must be specified"), 
                      j = NULL, alpha = NULL){
    m <- diag(n)
 
    if (!is.null(j) && !is.null(alpha)) {# E_ij(alpha): set (i,j) to alpha
        m[i, j] <- alpha
        return(m)
    }
    
    if (!is.null(c(j))){# E_ij: swap i and j rows
        tmp <- m[i, ]
        m[i, ] <- m[j, ]
        m[j, ] <- tmp
        return(m)
    }

    if (!is.null(alpha)){# E_i(alpha): multiply row i for alpha
        m[i, ] <- m[i, ] * alpha
        return(m)
    }
        
    stop("Something is wrong")
  
}
