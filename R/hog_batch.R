#' TITLE A Function To Create Multiple Histograms of Oriented Gradients (HoGs)
#'
#' DESCRIPTION This function does these things...
#'
#' DETAILS 
#'
#' Methods include:
#'
#' * this one
#'
#' * that one
#'
#' @param cfg_cells Numeric Vector. Defaults to 4:9
#' @param cfg_bins  Numeric Vector. Defaults to 7:15
#' @return Returns a list of HoGs
#' @export
#' @examples
#' hogs <- hog_batch( IMAGE_PATH = '/Users/Karl/Dropbox/Projects/Video-Captioning/pipelines/box2/')
#' hogs$names
#' str( hogs$values )

hog_batch <- function( IMAGE_PATH = './'
  , cfg_cells = 4:9
  , cfg_bins  = 7:15
){
  data <- list(
      cells = cfg_cells
    , bins = cfg_bins
  )

  # Create descriptions for HoGs
  ( data %>%
    purrr::cross() %>%
    purrr::map_chr( purrr::lift( paste )) %>%
    purrr::map_chr(
      ~gsub(
          '([0-9][0-9]*) ([0-9][0-9]*)'
        , '\\1 cells \\2 bins'
        , .x
      )
    ) -> hog_names
  )

  # Get all combinations of cells and bins
  data %>%
    purrr::cross_df() -> df

  # Create HoGs
  purrr::map2(
      .x = df$cells
    , .y = df$bins
    , .f = function( x, y ) OpenImageR::HOG_apply(
        IMAGE_PATH, cells = x, orientations = y
      )
  ) -> hogs

  #str( hogs )
  list( names=hog_names, values=hogs )
}

# multi_hog <- hog_batch()
# # Keep just the file names and the data
# filenames <- multi_hog$hogs[[ 1 ]]$files
# all_hogs <- map( multi_hog$hogs, 2 )

# Remove data no longer needed
# rm( hogs )   <---------- This might be making R console unresponsive

# -----------------------------------------------
