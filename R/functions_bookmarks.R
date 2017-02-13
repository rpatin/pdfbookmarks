#' Save Bookmarks
#'
#' \code{save.bookmark} save bookmarks.
#' @param text the data.frame with the different variable
#' @param bookmarks names of the variables on which statistics are calculated
#' @param level names of the variable with which states are ordered
#' @param page type of model either 'hmm' or 'picard'
#' @return  a bookmark list
#'
#' @examples
#' save.bookmark(text,bookmarks=list(),level=1,page=NULL)
#' @export
#'

save.bookmark <- function(text,bookmarks=list(),level=1,page=NULL) {
  if (!..device.set.up) {
    Cairo.onSave(device = dev.cur(),
                 onSave=function(device,page){
                   ..current.page <<- page
                 })
    ..device.set.up <<- TRUE
  }
  if (missing(page)|| is.null(page)) {
    page <- ..current.page+1
  }
  bookmarks[[length(bookmarks)+1]] <-
    list(text=text,
         level=level,
         page=page)
  return(bookmarks)
}

#' Set Bookmarks
#'
#' \code{set.bookmark} set bookmarks as null.
#' @return NULL
#'
#' @examples
#' set.bookmark()
#' @export
#'

set.bookmark <- function() {
  ..device.set.up <<- FALSE
  ..current.page <<- 0
  return(invisible(NULL))
}

#' Save Bookmarks
#'
#' \code{write.bookmarks} save bookmarks.
#' @param pdf.file the pdf file
#' @param bookmarks the bookmarks written by save.bookmarks
#' @return change pdf.file and adds bookmarks to it.
#'
#' @examples
#' write.bookmarks(pdf.file,bookmarks=list())
#' @export

write.bookmarks <- function(pdf.file,bookmarks=list()) {
  pdf.bookmarks <- ""
  for (bookmark in 1:length(bookmarks)) {
    pdf.bookmarks <-
      paste0(pdf.bookmarks,
             "BookmarkBegin\n",
             "BookmarkTitle: ",bookmarks[[bookmark]]$text,"\n",
             "BookmarkLevel: ",bookmarks[[bookmark]]$level,"\n",
             "BookmarkPageNumber: ",bookmarks[[bookmark]]$page,"\n")
  }
  temp.pdf <- paste(tempfile(pattern=basename(pdf.file)),".pdf",sep="")
  temp.pdf.info <- tempfile(pattern=paste0(basename(pdf.file),"info_utf8"))
  cat(file=temp.pdf.info,pdf.bookmarks)
  system2("pdftk",c(pdf.file,'update_info_utf8',temp.pdf.info,'output',temp.pdf))
  # file.remove(pdf.file)
  if (file.exists(temp.pdf)) {
    file.copy(temp.pdf,pdf.file,overwrite = T)
  } else {
    warning("unable to properly create bookmarks")
  }
}
