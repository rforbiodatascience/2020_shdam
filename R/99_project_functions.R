# Define project functions
# ------------------------------------------------------------------------------
get_mdls <- function(df){
  return( lm(weight ~ height, data = df) )
}