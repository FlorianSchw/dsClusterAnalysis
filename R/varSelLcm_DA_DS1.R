#'
#' @import VarSelLCM
#' @import dplyr
#' @export
#' 


varSelLcm_DA_DS1 <- function(df, num.clust, vbleSelec, crit.varsel, initModel, nbcores, nbSmall, iterSmall, nbKeep, iterKeep, tolKeep){
  
  df <- eval(parse(text=df), envir = parent.frame())  
  
  
  # Cuts the tree
  results <- VarSelLCM::VarSelCluster(x = df,
                                      gvals = num.clust,
                                      vbleSelec = vbleSelec,
                                      crit.varsel = crit.varsel,
                                      initModel = initModel,
                                      nbcores = nbcores,
                                      nbSmall = nbSmall,
                                      iterSmall = iterSmall,
                                      nbKeep = nbKeep,
                                      iterKeep = iterKeep,
                                      tolKeep = tolKeep) 
  

  results_values <- fitted(results)
  
  return(results_values)
  
  
  
}
