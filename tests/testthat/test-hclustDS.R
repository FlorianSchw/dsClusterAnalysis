
test_that("hclustDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,4)
  col2 <- c(3,29,23,15,-8)
  col3 <- c(-1,17,49,39,14)
  col4 <- c(1,1,1,0,1)

  test_df1 <- data.frame(col1, col2, col3, col4)

  # Creating results of distDS to compare to
  res1 <- distDS(df.name = "test_df1", method = "euclidean")
  clust1 <- hclustDS(diss = "res1", method = "ward.D2")
  
  res_expect1 <- c(1,5,2,3,4)
  res_expect2 <- c(17.6,22.1,33.3,58.6)
  
  
  # Actual Test Start  
  expect_equal(clust1[[2]], res_expect2, tolerance = 1e-1)
  expect_equal(clust1[[3]], res_expect1)
  expect_equal(clust1[[7]], "euclidean")

  
})






















