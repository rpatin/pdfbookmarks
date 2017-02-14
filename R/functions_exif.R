#' Get Exif information
#'
#' \code{getexif} get pdf information from files.
#' @param  file: the file to be updated
#' @param  exiftool: the path to the ExifTool binary
#' @param  opts: additional arguments to ExifTool (optional)
#' @param  intern: should a named vector of metadata be returned? (bool)
#' @param  simplify: if intern==TRUE, should the results be returned as a named
#'   vector (TRUE) or as a data.frame (FALSE)?
#' @return the pdf metadata
#'
#' @examples
#' getexif(file, exiftool='D:/remiPatin/Recherche/project/exiftool.exe', opts=NULL, intern=TRUE, simplify=FALSE)
#' @export
#'

getexif <- function(file, exiftool='D:/remiPatin/Recherche/project/exiftool.exe', opts=NULL,
                    intern=TRUE, simplify=FALSE) {
  # file: the file to be updated
  # exiftool: the path to the ExifTool binary
  # opts: additional arguments to ExifTool (optional)
  # intern: should a named vector of metadata be returned? (bool)
  # simplify: if intern==TRUE, should the results be returned as a named
  #           vector (TRUE) or as a data.frame (FALSE)?
  arg <- c(opts, normalizePath(file))
  if(intern) {
    exif <- system2(normalizePath(exiftool), args=arg, stdout=TRUE)
    exif <- do.call(rbind, strsplit(exif, ' +: +', perl=T))
    row.names(exif) <- exif[, 1]
    exif[, 2, drop=simplify]
  } else {
    system2(normalizePath(exiftool), args=arg, stdout='')
  }
}


#' Get Exif information
#'
#' \code{getexif} get pdf information from files.
#' @param  file: the file to be updated
#' @param  metadata: a named character vector or list containing metadata
#' @param  exiftool: the path to the ExifTool binary
#' @return  write down the new pdf file.
#'
#' @examples
#' setexif(file, metadata, exiftool='D:/remiPatin/Recherche/project/exiftool.exe')
#' @export

setexif <- function(file, metadata, exiftool='D:/remiPatin/Recherche/project/exiftool.exe') {
  # file: the file to be updated
  # metadata: a named character vector or list containing metadata
  # exiftool: the path to the ExifTool binary
  exif <- sprintf('-%s="%s"', names(metadata), metadata)
  system2(exiftool, args=c(exif, file))
  file.remove(paste(file,"_original",sep=""))
}
