#' A Function to Create a Collage of Images
#'
#' This function creates a collage from the given graphic object (Grobs)
#'
#' @param graphic_objects Vector of grobs Images to include in collage
#' @param collage_file_name Character Vector. Defaults to 'Collage%03d.png'
#' @return Returns NULL; Saves the collage image as a side effect.
#' @examples
#'
#' filenames[ clusterID == thisNumber ] %>%
#' map( ~get_raster_grob( .x )     ) -> grobs
#' collage_of_grobs( grobs )
#' @export
collage_of_grobs <- function( graphic_objects, collage_file_name = 'Collage%03d.png' ){
  NUM_COLUMNS <- 5
  ggsave(
      collage_file_name
    , width  = 8.5
    , height = 11
    , gridExtra::marrangeGrob(
          grobs = graphic_objects
        , nrow  = 1 + round( length( graphic_objects ) / NUM_COLUMNS, 0 )
        , ncol  = NUM_COLUMNS
        , top   = NULL
      )
  )    
  NULL
}

get_img         <- function( f ) png::readPNG( paste0( img_path, f ) )
get_raster      <- function( f ) as.raster( get_img( f ) )
get_raster_grob <- function( f ) grid::rasterGrob( get_raster( f ), interpolate = FALSE )
