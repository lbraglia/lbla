#' Immaginary unit
#'
#' Immaginary unit made to simplify writing complex numbers
#' 
#' @export
i <- 1i


#' Make a matrix by row quickly
#'
#' Make a matrix by row quickly
#' 
#' @param ... params passed to matrix
#' @export
rmatrix <- function(...) matrix(..., byrow = TRUE)


#' Check wheter a matrix like object is square
#'
#' Check wheter a matrix like object (has ncol and nrow) is square
#' 
#' @param x a matrix-like object
#' @export
is_square <- function(x) ncol(x) == nrow(x)


#' Power of a matrix
#'
#' Calculate the power of a matrix
#' 
#' @param x a matrix-like object
#' @param n exponent
#' @export
matrix_pow <- function(x, n){
    if(!is_square(x))
        stop("x must be a square matrix")
    n <- as.integer(n)
    matr_order <- nrow(x)

    if (n == 0L){
        diag(matr_order)
    } else if (n == 1L) {
        x
    } else {
        res <- x
        for (i in seq_len(n-1)){
            res <- res %*% res
        }
        res
    }
    
}


#' H-transpose
#'
#' H-transpose a matrix
#' 
#' @param x a matrix
#' @export
ht <- function(x) Conj(t(x))

#' check (matrix) equality in a safe manner
#'
#' check equality in safe manner
#' @param ... arguments to be tested
#' @export
all_equal <- function(...) isTRUE(all.equal(...))
