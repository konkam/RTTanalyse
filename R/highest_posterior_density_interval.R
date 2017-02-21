#' Kruschke, J. (2014). Doing Bayesian data analysis: A tutorial introduction with R, 2nd Ed. Academic Press.
#' @param probMassVec is a vector of probability masses at each grid point.
#' @param credMass is the desired mass of the HDI region.
#' @return value: A list with components:
#' indices is a vector of indices that are in the HDI
#' mass is the total mass of the included indices
#' height is the smallest component probability mass in the HDI
#'
#' @example For determining HDI of a beta(30,12) distribution
#' approximated on a grid:
#'   probDensityVec = dbeta( seq(0,1,length=201) , 30 , 12 )
#'  probMassVec = probDensityVec / sum( probDensityVec )
#'   HDIinfo = HDIofGrid( probMassVec )
#'   show( HDIinfo )
HDIofGrid = function( probMassVec , credMass=0.95 ) {
  sortedProbMass = sort( probMassVec , decreasing=TRUE )
  HDIheightIdx = min( which( cumsum( sortedProbMass ) >= credMass ) )
  HDIheight = sortedProbMass[ HDIheightIdx ]
  HDImass = sum( probMassVec[ probMassVec >= HDIheight ] )
  return( list( indices = which( probMassVec >= HDIheight ) ,
                mass = HDImass , height = HDIheight ) )
}
