generate_data <- function(
    diff = 1,
    n = 10,
    s = 1000
) {
  # synchro
  set.seed(42)
  # simulation 1st group
  gr1 <- rnorm( n * s )
  # simulation 2nd group
  gr2 <- rnorm( n * s, mean = diff )
  # insert values into a matrix 2D
  res <- matrix( c( gr1, gr2 ), nrow = 2 * n, ncol = s, byrow = TRUE)
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
  mat2 <- mat[ 1:n + n, ]
  mean2 <- apply( mat2, 2, mean )
  # determine limits
  xlimits <- range( c( mean1, mean2 ) )
  # plot 1st histogram
  hist( mean1, col = "lightblue", xlim = xlimits )
  # plot 2nd histogram
  hist( mean2, add = TRUE, col = "#FFAAAA88")
}



calc_pvalue <- function(
    X, # matrix of variables
    Y # response
) {
  # debug
  # X <- tmp
  #
  # indices_gr1
  # indices_gr2
  # calculate t.test de la 15e variable = 15e colonne
  tt <- t.test( X[ 1:n, 15 ], X[ 1:n + n, 15] )
  tt$p.value

  res <- apply( X, 2, function( vec_col ) {
    tt <- t.test( vec_col[ 1:n ], vec_col[ 1:n + n] )
    tt$p.value
  })
  return( res )
}



# experiences virtuelles

# difference 1
X_diff_1 <- generate_data( diff = 1 )
# tt le monde doit avoir les memes valeurs
X_diff_1[11:13,1]
plot_hist( X_diff_1 )


# difference 0
X_diff_0 <- generate_data( diff = 0 )
# tt le monde doit avoir les memes valeurs
X_diff_0[11:13,1]
plot_hist( X_diff_0 )



# devel

tmp <- generate_data()
# tt le monde doit avoir les memes valeurs
tmp[1:3,1]

plot_hist( tmp )

pheatmap::pheatmap(X, cluster_rows = FALSE, cluster_cols = FALSE)

n <- 10
X <- tmp



