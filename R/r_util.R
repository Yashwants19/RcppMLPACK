CLIGetParamMat <- function(paramName, paramValue)
{
  mat = CLI_GetParamMat(as.character(paramName))
  nrows = CLI_GetParamMatRows(as.character(paramName))
  ncols = CLI_GetParamMatCols(as.character(paramName))
  return (matrix(mat, nrows, ncols))
}
