#' Generate a matrix of measurement
#'
#' @param diff difference between the average of two groups
#' @param n_indiv number of individuals per group
#' @param n_var number of variables
#' @param prop proportion of variables that have a difference
#' @param seed seed for random generator
#'
#' @returns a matrix
#' @export
#'
#' @examples
generate_data <- function(
    diff = 1,
    n_indiv = 10,
    n_var = 1000,
    prop = 1,
    seed = 42
) {
  # synchro
  set.seed(seed)
  # simulation 1st group
  gr1 <- matrix( rnorm( n_indiv * n_var ), nrow = n_indiv, byrow = TRUE)
  rownames( gr1 ) <- sprintf( "g1%02d", 1:n_indiv )
  # simulation 2nd group
  gr2 <- matrix( rnorm( n_indiv * n_var ), nrow = n_indiv, byrow = TRUE)
  # add the difference in a proportion of variables
  n_var_diff <- floor( n_var * prop )
  gr2[ , 1:n_var_diff ] <- gr2[ , 1:n_var_diff ] + diff
  rownames( gr2 ) <- sprintf( "g2%02d", 1:n_indiv )
  # insert values into a matrix 2D
  res <- rbind( gr1, gr2 )
  colnames( res ) <- sprintf( "v%04d", 1:n_var )
  return(res)
}

#' Plot a heatmap revealing the data inside a matrix
#'
#' @param mat a matrix of measurements, individuals as rows, variables as
#'   columns
#'
#' @returns
#' @export
#'
#' @examples
plot_heatmap <- function(
    mat,
    ...
) {
  pheatmap::pheatmap(
    mat, show_colnames = FALSE,
    cluster_rows = FALSE, cluster_cols = FALSE,
    ...
  )
}

# Debug
# X <- generate_data()
# plot_heatmap( X )

# Debug
# X <- generate_data( prop = 0.1 )
# plot_heatmap( X )


#' Plot the histogram of mean values per variable per group
#'
#' @param mat
#' @param breaks
#'
#' @returns
#' @export
#'
#' @examples
plot_hist <- function(
    mat,
    bin_width = 0.1,
    Y
) {
  if ( missing( Y ) ) {
    n <- nrow( mat ) / 2
    Y <- rep( c( "G1", "G2" ), each = n)
  }
  Y <- factor( Y )
  # indices of group 1 & 2
  indices_gr1 <- which( Y == levels( Y )[ 1 ])
  indices_gr2 <- which( Y == levels( Y )[ 2 ])
  # calculate mean gr1
  mat1 <- mat[ indices_gr1, ]
  mean1 <- apply( mat1, 2, mean )
  # calculate mean gr2
  mat2 <- mat[ indices_gr2, ]
  mean2 <- apply( mat2, 2, mean )
  # determine limits
  xlimits <- range( c( mean1, mean2 ) )
  xlimits[1] <- floor( xlimits[1] * 10 - 1) / 10
  xlimits[2] <- ceiling( xlimits[2] * 10 + 1) / 10
  breaks <- seq( xlimits[1] + 1e-6, xlimits[2] - 1e-6, bin_width )
  # plot 1st histogram
  hist( mean1, col = "#00CCBB33", breaks = breaks )
  # plot 2nd histogram
  hist( mean2, add = TRUE, col = "#CC00BB33", breaks = breaks )
}

# Debug
# plot_hist( X )


#' Calculate p-value for each variable of the measurement matrix
#'
#' @param X
#' @param Y
#'
#' @returns
#' @export
#'
#' @examples
calc_pvalue <- function(
    X,
    Y
) {
  if ( missing( Y ) ) {
    Y <- rep( c( "G1", "G2" ), each = nrow( X ) / 2)
  }
  Y <- factor( Y )
  # indices of group 1 & 2
  indices_gr1 <- which( Y == levels( Y )[ 1 ])
  indices_gr2 <- which( Y == levels( Y )[ 2 ])
  # calculate t.test de la 15e variable = 15e colonne
  # tt <- t.test( X[ 1:n, 15 ], X[ 1:n + n, 15] )
  # tt$p.value
  # apply to each column
  res <- apply( X, 2, function( vec_col ) {
    tt <- t.test( vec_col[ indices_gr1 ], vec_col[ indices_gr2 ] )
    tt$p.value
  })
  return( res )
}

# Debug
# X_pvalue <- calc_pvalue( X )
