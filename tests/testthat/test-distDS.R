
test_that("distDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,4)
  col2 <- c(3,29,23,15,-8)
  col3 <- c(-1,17,49,39,14)
  col4 <- c(1,1,1,0,1)
  col5 <- c(0,9,NA,13,NA)
  
  test_df1 <- data.frame(col1, col2, col3, col4)
  test_df2 <- data.frame(col1, col2, col3, col5)
  
  # Creating results of distDS to compare to
  res1 <- distDS(df.name = "test_df1", method = "euclidean")
  res2 <- distDS(df.name = "test_df2", method = "euclidean")
  
  m1 <- as.matrix(res1)
  m2 <- as.matrix(res2)
  
  
  outcome_correct1 <- matrix(c(0.00000, 32.18695, 53.88877, 44.05678, 22.13594,
                               32.18695, 0.00000, 32.80244, 27.29469, 37.60319,
                               53.88877, 32.80244, 0.00000, 17.57840, 47.81213,
                               44.05678, 27.29469, 17.57840, 0.00000, 34.04409,
                               22.13594, 37.60319, 47.81213, 34.04409, 0.00000), 
                             nrow = 5, ncol = 5, byrow = TRUE, 
                             dimnames = list(c("1", "2", "3", "4", "5"),
                                             c("1", "2", "3", "4", "5")))

  outcome_correct2 <- matrix(c(0.00000, 33.42155, 62.22540, 45.92385, 25.56039,
                               33.42155, 0.00000, 37.87699, 27.56810, 43.42043,
                               62.22540, 37.87699, 0.00000, 20.26491, 55.20869,
                               45.92385, 27.56810, 20.26491, 0.00000, 39.29377,
                               25.56039, 43.42043, 55.20869, 39.29377, 0.00000), 
                             nrow = 5, ncol = 5, byrow = TRUE, 
                             dimnames = list(c("1", "2", "3", "4", "5"),
                                             c("1", "2", "3", "4", "5")))
  
  
  # Actual Test Start  
  expect_equal(attr(res1, "Size"), 5)
  expect_equal(attr(res1, "Diag"), FALSE)
  expect_equal(attr(res1, "Upper"), FALSE)
  expect_equal(attr(res1, "method"), "euclidean")
  expect_equal(class(res1), "dist")
  
  expect_equal(attr(res2, "Size"), 5)
  expect_equal(attr(res2, "Diag"), FALSE)
  expect_equal(attr(res2, "Upper"), FALSE)
  expect_equal(attr(res2, "method"), "euclidean")
  expect_equal(class(res2), "dist")
  
  expect_equal(m1, outcome_correct1, tolerance = 1e-2)
  expect_equal(m2, outcome_correct2, tolerance = 1e-2)

})



