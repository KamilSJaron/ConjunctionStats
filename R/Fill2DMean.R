#' @title Fill2DMean
#'
#' @description
#' \code{Fill2DMean}
#'
#' @param GradTable is a filled summary of 2D simulations (usually output of \code{FillSetting})
#'
#' @param stat 'width' to compute mean and variance of widths,
#' 'center' to compute mean and variance of centers and 'both' for both.
#'
#' @param filter is the treshold of widths that will be filtered out (usually estimates < 1 are meaningless)
#'
#' @param method to get mean (mean, hmean or any other funcion that tranform vector of numbers into one and takes rm.na = T argument)
#'
#' @param altGradTable is a summary of the same 2D simulations with independent width/center estimates, these values are used in to fill failed estimates (< filter)
#'
#' @return The input GradTable with computed mean and variance of widths, centers or both
#'
#' @author Kamil Jaron \email{kamiljaron at gmail.com}
#'
#' @export

Fill2DMean <- function(GradTable, stat = 'both', filter = 1, method = hmean, altGradTable = NA){

  if(stat == 'both'){
    GradTable <- Fill2DMean(GradTable, 'width', filter, method, altGradTable)
    GradTable <- Fill2DMean(GradTable, 'center', 0, method, altGradTable)
    return(GradTable)
  }

  stat_cmp <- paste0(stat,'_')
  stat_length <- nchar(stat_cmp)

# deleted which, I guess it will work, not sure
  selected_cols <- substr(colnames(GradTable), 1, stat_length) == stat_cmp
  z <- as.matrix(GradTable[, selected_cols])

  if(!all(is.na(altGradTable))){
    selected_cols <- substr(colnames(altGradTable), 1, stat_length) == stat_cmp
    z_alt <- as.matrix(altGradTable[, selected_cols])
    z <- ifelse(z < filter, z, z_alt)
  }

  z[z < filter] <- NA
  GradTable[, stat] <- apply(z, 1, method, na.rm = T)
  GradTable[, paste0('var_',stat)] <- apply(z, 1, var, na.rm = T)

  return(GradTable)
}
