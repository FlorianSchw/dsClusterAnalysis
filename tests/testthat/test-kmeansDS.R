
test_that("kmeansDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,4,-2,31,49,-1,47)
  col2 <- c(3,29,23,15,-8,50,-1,17,17,47)
  col3 <- c(-1,17,49,39,14,27,12,44,-7,22)
  col4 <- c(1,1,1,0,1,0,1,1,0,1)
  col5 <- c(0,9,-4,27,29,2,9,NA,13,30)
  
  test_df1 <- data.frame(col1, col2, col3, col4)
  test_df2 <- data.frame(col1, col2, col3, col5)
  
  # Creating kmeansDS results to compare to
  res1 <- kmeansDS(df.name = test_df1, clusters = 2, iter.max = 5, nstart = 1, algorithm = "Hartigan-Wong", trace = FALSE, seed = 123)
  outcome_correct1 <- c(2,2,2,1,2,1,2,2,1,2)
  
  # Actual Test Start  
  expect_equal(res1, outcome_correct1)
  expect_error(kmeansDS(test_df2, 2, iter.max = 5, nstart = 1, algorithm = "Hartigan-Wong", trace = FALSE, seed = 123), "The data frames contains NAs.", fixed = TRUE)
  
})







