#' A Function To Create Multiple Image Hashes
#'
#' This function does these things:
#' (1) Create descriptions for Hashes
#' (2) Get all combinations of hash size, resizing method, and hashing method
#' (3) Create Hashes for each image using each configuration.
#'
#' @param cfg_hashsize Numeric Vector. Defaults to c( 4:6,8:10 ). NOTE: hash size of 7 crashes R !!!
#' @param cfg_hashmethod Vector of Character Vector. Defaults to c( 'average_hash', 'dhash', 'phash' )
#' @param cfg_hashresize Vector of Character Vector. Defaults to c( 'nearest', 'bilinear' )
#' @return Returns a list of hashes
#' @export
#' @examples
#' hashes <- hash_batch('/Users/Karl/Dropbox/Projects/Video-Captioning/pipelines/box2/')
#' hashes$names
#' hashes$values

hash_batch <- function( IMAGE_PATH = './'
  , cfg_hashsize   = c( 4:6,8:10 )
  , cfg_hashmethod = c( 'average_hash', 'dhash', 'phash' )
  , cfg_hashresize = c( 'nearest', 'bilinear' )
){
  hash_cfg <- list(
      hashsize   = cfg_hashsize
    , hashdescr  = paste( 'length', cfg_hashsize )
    , hashresize = cfg_hashresize
    , hashmethod = cfg_hashmethod
  )

  # Create descriptions for Hashes
  hash_cfg[2:4] %>% # omit hash_cfg[ 1 ], since hash_cfg[ 2 ] is a description of it 
    purrr::cross()     %>%
    purrr::map_chr( purrr::lift( paste )) -> hash_names

  # Get all combinations of hash size, resizing method, and hashing method
  hash_cfg[ c( 1, 3, 4 ) ] %>% # Now use hash_cfg[ 1 ] ( numeric ) and omit hash_cfg[ 2 ] ( character )
    purrr::cross_df() -> hash_df

  # Create Hashes for each image using each configuration
  purrr::map2(
      .x = hash_df$hashsize
    , .y = hash_df$hashmethod
    , .f = function( x, y ) OpenImageR::hash_apply(
               IMAGE_PATH
             , hash_size = x
             , method    = y
             , mode      = 'binary' # c( 'hash'   , 'binary' )
             , threads   = 1
             , resize    = 'nearest' # c( 'nearest', 'bilinear' )
           )
  ) -> hashes
 
  list( names = hash_names, values = hashes )
}
