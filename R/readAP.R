#' Read mavlink taskfiles
#'
#' @description Reads a UAV mavlink flight task 
#'
#' @param taskfile filepath of the task
#' @param spatial boolean, return as sf, default FALSE
#'
#' @author Marvin Ludwig
#' 
#' @return a list with 1. a data.frame with the waypoints and 2. the mavlink header
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' mav = readAP(system.file("extdata", "MAVLINK_waypoints.txt", package = "PixhawkPlanner"), spatial = TRUE)
#' mapview::mapview(mav$task)
#' 
#' }
#' 
#' 
#'

readAP <- function(taskfile, spatial = FALSE){

  h = readLines(taskfile, n = 1)
  t <- read.table(taskfile, skip = 1, header = FALSE, sep = "\t")


  colnames(t) = c("Index", "CurrentWP", "CoordFrame", "Command", "Param1", "Param2", "Param3", "Param4",
                  "Longitude", "Latitude", "Altitude", "Autocontinue")


  if (spatial == TRUE){
    t <- sf::st_as_sf(t, coords = c("Latitude", "Longitude", "Altitude"), dim = "XYZ",crs = 4326)
  }

  return(list(task = t, header = h))

}

