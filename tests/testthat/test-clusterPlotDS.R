

test_that("clusterPlotDS errors", {
  
  # Creating differing test data.frames -  preparation
  col1 <- c(16,10,14,2,41,1,0,1,8,7)
  col2 <- c(3,29,23,15,-8,16,10,14,-20,11)
  col3 <- c(-1,17,49,39,14,23,15,-8,43,78)
  col4 <- c(1,1,1,0,1,8,7,12,14,151)
  col5 <- c(3,9,-1,NA,0,10,14,-42,8,10)
  
  test_df1 <- data.frame(col1, col2, col3, col4)
  test_df2 <- data.frame(col1, col2, col3, col5)
  
  # Creating results of clusterPlotDS based on ds.dist and ds.hclust
  res1 <- distDS(df.name = "test_df1", method = "euclidean")
  clust1 <- hclustDS(diss = "res1", method = "ward.D2")
  
  res2 <- distDS(df.name = "test_df2", method = "euclidean")
  clust2 <- hclustDS(diss = "res2", method = "ward.D2")
  
  result1 <- clusterPlotDS(tree = "clust1", k = 4, h = NULL, k_colors = NULL, palette = NULL, show_labels = TRUE, color_labels_by_k = FALSE)
  result2 <- clusterPlotDS(tree = "clust2", k = NULL, h = 50, k_colors = NULL, palette = NULL, show_labels = TRUE, color_labels_by_k = FALSE)

  # Actual Test Start  
  expect_silent(result1)
  expect_silent(result2)

})

