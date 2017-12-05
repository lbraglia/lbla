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


#' check if tho matrix have same dimensions (nrow, ncol)
#'
#' check if tho matrix have same dimensions (nrow, ncol)
#' 
#' @param x the first matrix
#' @param y the second matrix
#' @export
same_dimensions <- function(x, y){
    all(dim(x) == dim(y))
}


#' check (matrix) equality in a safe manner
#'
#' check equality in safe manner
#' @param ... arguments to be tested
#' @param x the first matrix
#' @param y the second matrix
#' @export
equal_matrix <- function(x, y) {
    same_dimensions(x, y) && isTRUE(all.equal(x, y))
}

#' check if a matrix is symmetric
#'
#' check if a matrix is symmetric
#' @param x the matrix
#' @export
is.symmetric <- function(x) equal_matrix(x, t(x))


#' check if a matrix is hermitian
#'
#' check if a matrix is hermitian
#' @param x the matrix
#' @export
is.hermitian <- function(x) equal_matrix(x, ht(x))


#' check if a matrix is antisymmetric
#'
#' check if a matrix is antisymmetric
#' @param x the matrix
#' @export
is.antisymmetric <- function(x) equal_matrix(x, - t(x))


#' check if a matrix is antihermitian
#'
#' check if a matrix is antihermitian
#' @param x the matrix
#' @export
is.antihermitian <- function(x) equal_matrix(x, - ht(x))
