#' A Function To Cluster hashes and HoGs
#'
#' This function does these things...
#'
#' DETAILS 
#'
#' @param features_to_cluster Numeric Vector
#' @param filenames Vector of Character Vector
#' @param algorithm Character Vector. Defaults to c( 'adaptive' )
#' @param N Numeric Vector. Number of clusters (ignored for adaptive; otherwise defaults to 2)
#' @return Returns a list of cluster IDs and cluster exemplar filenames
#' @export
#' @examples
#' img_path <- '/Users/Karl/Dropbox/Projects/Video-Captioning/pipelines/box2/'
#' image_filenames <- dir( path=img_path )
#' clusters <- get_clusters( data = hogs[[2]][[1]]$hog, filenames = image_filenames )
#' clusters$IDs
#' clusters$exemplars

get_clusters <- function( data, filenames, algorithm = 'adaptive', N = 2 ){
  clusters <- exemplar_image_names <- NULL
  
  if( algorithm == 'adaptive' ){
    # adaptive clustering
    clusters <- ADPclust::adpclust( data )
    seq( clusters$nclust ) %>%
      purrr::map_int( ~clusters$centers[ .x ] ) -> indexes
  }
  
  if( algorithm == 'kmeans' ){
    # K-means clustering
    clusters <- kmeans( data, centers = N )
    seq( N ) %>%
      purrr::map_int( ~match( .x, clusters$cluster )) -> indexes
  }
  
  exemplar_image_names <- if( any( grepl( 'indexes', ls() )) ) purrr::map_chr( indexes, ~filenames[ .x ] )

  try(
    if( length( clusters ) > 0 ){
      # --- Get the name of the variable used to identify clusters
      cluster_var <- grep( 'cluster*', names( clusters ), value=TRUE )

      # --- Now use the cluster variable to get cluster IDs
      clusters <- clusters[[ cluster_var ]]
    }
  )
  list( IDs=clusters, exemplars=exemplar_image_names )
}
