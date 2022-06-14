#' Convert a tfrmt into template function
#'
#' @param tfrmt_obj
#'
#'
as_tfrmt_template <- function(tfrmt,...){


  function(tfrmt_obj, ...){
    if(!missing(tfrmt_obj)){
      tfrmt <- layer_tfrmt(tfrmt_obj, tfrmt)
    }
    trfmt
  }


}

# tplyr_demog_tfrmt <- tfrmt()
