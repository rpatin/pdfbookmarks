% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_bookmarks.R
\name{write.bookmarks}
\alias{write.bookmarks}
\title{Save Bookmarks}
\usage{
write.bookmarks(pdf.file, bookmarks = list())
}
\arguments{
\item{pdf.file}{the pdf file}

\item{bookmarks}{the bookmarks written by save.bookmarks}
}
\value{
change pdf.file and adds bookmarks to it.
}
\description{
\code{write.bookmarks} save bookmarks.
}
\examples{
write.bookmarks(pdf.file,bookmarks=list())
}

