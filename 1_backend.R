generate_data <- function(
    diff = 1,
    n = 10,
    s = 1000
) {
  #
  # simulation 1st group
  gr1 <- rnorm( n * s )
  # simulation 2nd group
  gr2 <- rnorm( n * s, mean = diff )
  # insert values into a matrix 2D
  res <- matrix( c( gr1, gr2 ), nrow = 2 * n, ncol = s, byrow = TRUE)
  # check heatmap
  pheatmap::pheatmap(res, cluster_rows = FALSE, cluster_cols = FALSE)
  return(res)
}

plot_hist <- function(
    mat
) {
  # compute n
  n <- nrow( mat ) / 2
  # calculate mean gr1
  mat1 <- mat[ 1:n, ]
  mean1 <- apply( mat1, 2, mean )
  # calculate mean gr2
  # plot 1st histogram
  hist()
  # plot 2nd histogram
  hist(, add = TRUE)
}
