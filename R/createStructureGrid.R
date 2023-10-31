#' Create structure grid
#'
#' Create a structure grid of squares and count the number of structures in each one
#' @param structures File containing structures' polygons
#' @param cellSize Length of edge of grid squares, in meters
#' @param mode Mode to adopt when counting structures in multiple squares: "maximal" to count each occurrence, "minimal" to count it just in square covering most of its surface
#' @param internalBuffer Distance from border of grid containing structures to be excluded, in meters. Default value is FALSE
#' @param xmin,ymin Specify a corner of portion of structures' file to consider
#' @param xmax,ymax Specify opposite corner of portion of structures' file to consider
#' @param templateGrid A raster file with the grid to use for counting the structures: if not NULL, cellSize is ignored
#' @return Grid of squares with edge cellSize or with the same characteristics as the templateGrid, with count of associated structures
#' @examples
#' grid <- createBuildingGrid <- function(structures, cellSize = 100, mode = "maximal")
#' @export
#'
createStructureGrid <- function(structures,
                                cellSize = 100,
                                mode = "maximal",
                                internalBuffer = F,
                                xmin = NULL,
                                xmax = NULL,
                                ymin = NULL,
                                ymax = NULL,
                                templateGrid = NULL) {
  require(sf)
  require(dplyr)
  require(raster)
  require(terra)
  
  if (!(mode %in% c("maximal", "minimal"))) {
    stop('Mode accepts only "maximal" or "minimal"')
  }
  
  if (is.null(templateGrid)) {
    if (is.null(xmin) |
        is.null(xmax) | is.null(ymin) | is.null(ymax)) {
      bboxStructures <- st_bbox(structures)
    } else{
      bboxStructures <- st_bbox(c(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      crs = st_crs(4326))
    }
  } else{
	bboxStructures <- st_bbox(templateGrid, crs = st_crs(4326))
	# g_grid <- rasterToPolygons(templateGrid)
    # g_grid <- st_as_sf(g_grid)
    # g_grid <- st_transform(g_grid, st_crs(buildings))
	}
    g_poly <- st_as_sfc(bboxStructures) %>%
      st_transform(st_crs(structures))
    g_grid <- st_make_grid(g_poly, cellsize = cellSize)
    g_grid <- st_as_sf(g_grid)

  
  if (mode == "maximal") {
    Sum_Count <- lengths(st_intersects(g_grid, structures, sparse = T))
  } else{
    temp <- suppressWarnings(st_intersection(structures, g_grid)) %>%
      mutate(area = st_area(.) %>% as.numeric()) %>%
      group_by(OBJECTID) %>% # this part is fragile, solve
      filter(area == max(area))
    Sum_Count <- lengths(st_contains(g_grid, temp, sparse = T))
  }
  
  g_grid$Sum_Count <- Sum_Count
  st_geometry(g_grid) <- "geometry"
  
  if (internalBuffer) {
    st_buffered <- st_buffer(st_as_sf(g_poly),-internalBuffer)
    g_grid$Sum_Count[c(!st_contains(st_buffered, g_grid, sparse = F))] <-
      0
  }
  
  print("DONE")
  return(g_grid)
}
