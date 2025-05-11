#' Engine wear versus size data from gamair (Wood, 2007)
#'
#' Data on engine wear against engine size for 19 Volvo car engines.
#' @name engine
#' @docType data
#' @format A data frame with 2 columns and 19 rows. Each row refers to one engine model. The columns are:
#' \describe{
#'   \item{wear}{an index of engine wear rate.}
#'   \item{size}{cylinder capacity in litres.}
#' }
#' 
#' @source {gamair} Originally from... http://www3.bc.sympatico.ca/Volvo_Books/engine3.html
NULL

#' Brain scan data from gamair (Wood, 2007)
#'
#' Functional magnetic resonance imaging measurements for a human brain subject to a particular experimental stimulus. One slice of the image is provided, 
#' described as a near-axial slice through the dorsal cerebral cortex.
#' @name brain
#' @docType data
#' @format A data frame with 5 columns and 1567 rows. Each row refers to one ‘voxel’ of the image. The columns are:
#' \describe{
#'   \item{X}{voxel position on horizontal axis.}
#'   \item{Y}{voxel position on vertical axis.}
#'   \item{medFPQ}{median of three replicate 'Fundamental Power Quotient' values at the voxel: this is the main measurement of brain activivity.}
#'   \item{region}{code indicating which of several regions of the brain the voxel belongs to. The regions are defined by the experimenters. 0 is the base region; 1 is the region of interest; 2 is the region activated by the experimental stimulus; NA denotes a voxel with no allocation.}
#'   \item{meanTheta}{mean phase shift at the Voxel, over three measurments.}
#' }
#' 
#' @source {gamair} S. Landau et. al. (2003). ‘Tests for a difference in timing of physiological response between two brain regions measured by using functional magnetic resonance imaging’. Journal of the Royal Statistical Society, Series C, Applied Statistics, 53(1):63-82
NULL

#' Rodent Captures from the Portal Project provided by mvgam package (Ernest et. al., 2018; Clark, 2024)
#'
#' A dataset containing timeseries of total captures (across all control plots) for select rodent species from the Portal Project
#' @name portal_data
#' @docType data
#' @format A data frame with columns:
#' \describe{
#'   \item{time}{time of sampling, in lunar monthly cycles}
#'   \item{series}{factor indicator of the time series, i.e. the species}
#'   \item{captures}{total captures across all control plots}
#'   \item{mintemp}{monthly mean minimum temperature}
#'   \item{ndvi_ma12}{12-month moving average of the mean Normalised Difference Vegetation Index}
#' }
#' 
#' @source {mvgam} https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md
NULL
