
test_that("nbclustDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,4,2,1,3,-6,9)
  col2 <- c(3,29,23,15,-8,10,14,2,4,2)
  col3 <- c(-1,17,49,39,14,29,23,15,-8,-9)
  col4 <- c(1,1,1,0,1,0,0,0,1,1)
  
  test_df1 <- data.frame(col1, col2, col3, col4)
  
  # Creating results of nbclustDS to compare to
  nmbr_clust1 <- nbclustDS(df.name = "test_df1", diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 5, method = "ward.D2", index = "all", alphaBeale = 0.1, seed = 123)
  
  
  res_expect1 <- c(1,2,2,2,1,2,2,1,1,1)
  

  # Actual Test Start  
  expect_equal(nmbr_clust1[[4]], res_expect1)

})

















