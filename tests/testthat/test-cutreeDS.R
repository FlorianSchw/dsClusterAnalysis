


test_that("cutreeDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,4)
  col2 <- c(3,29,23,15,-8)
  col3 <- c(-1,17,49,39,14)
  col4 <- c(1,1,1,0,1)
  
  test_df1 <- data.frame(col1, col2, col3, col4)
  
  # Creating results of cutreeDS to compare to
  res1 <- distDS(df.name = "test_df1", method = "euclidean")
  clust1 <- hclustDS(diss = "res1", method = "ward.D2")
  tree1 <- cutreeDS(tree = "clust1", 2)
  
  res_expect1 <- c(1,2,2,2,1)

  # Actual Test Start  
  expect_equal(tree1, res_expect1)
  expect_equal(length(tree1), 5)

})






































