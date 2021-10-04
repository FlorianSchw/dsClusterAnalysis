



test_that("nbclustVisualDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,41,1,0,1,8,7)
  col2 <- c(3,29,23,15,-8,16,10,14,-20,11)
  col3 <- c(-1,17,49,39,14,23,15,-8,43,78)
  col4 <- c(1,1,1,0,1,8,7,12,14,151)
  col5 <- c(3,9,-1,NA,0,10,14,-42,8,10)
  
  test_df1 <- data.frame(col1, col2, col3, col4)
  test_df2 <- data.frame(col1, col2, col3, col5)
  
  
  # Actual Test Start  
  expect_silent(nbclustVisualDS(df.name = "test_df1", FUNcluster = "kmeans", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 8, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE))
  expect_silent(nbclustVisualDS(df.name = "test_df1", FUNcluster = "hcut", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 8, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE))
  expect_silent(nbclustVisualDS(df.name = "test_df1", FUNcluster = "cluster::pam", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 8, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE))
  expect_silent(nbclustVisualDS(df.name = "test_df1", FUNcluster = "cluster::fanny", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 4, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE))
  expect_silent(nbclustVisualDS(df.name = "test_df1", FUNcluster = "cluster::clara", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 8, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE))
  
  expect_error(nbclustVisualDS(df.name = "test_df2", FUNcluster = "kmeans", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 8, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE), "The data frames contains NAs.", fixed = TRUE)
  expect_error(nbclustVisualDS(df.name = "test_df1", FUNcluster = "hcut", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 11, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE), "For this clustering method 'k.max' must be between 1 and nrow(df.name).", fixed = TRUE)
  expect_error(nbclustVisualDS(df.name = "test_df2", FUNcluster = "cluster::pam", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 12, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE), "For this clustering method 'k.max' must be between 1 and nrow(df.name).", fixed = TRUE)
  expect_error(nbclustVisualDS(df.name = "test_df2", FUNcluster = "cluster::clara", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 8, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE), "The data frames contains NAs.", fixed = TRUE)
  expect_error(nbclustVisualDS(df.name = "test_df2", FUNcluster = "cluster::fanny", method = c("silhouette", "wss", "gap_stat"), diss = NULL, k.max = 5, nboot = 100, verbose = interactive(), barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", print.summary = TRUE), "For this clustering method 'k.max' must be in 1,2, .., n/2 -1.", fixed = TRUE)

})



