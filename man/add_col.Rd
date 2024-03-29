% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_col.R
\name{add_col}
\alias{add_col}
\title{Add a colum to a data frame}
\usage{
add_col(x, name, value, where = ncol(x) + 1)
}
\arguments{
\item{x}{A \link[base:data.frame]{base::data.frame}}

\item{name}{Name of variable to create. If variable of that name
already exists}

\item{value}{Values to insert.}

\item{where}{position to insert. Use 1 to insert on LHS. Default RHS.}
}
\description{
Similar to \link[base:cbind]{base::cbind}, but allows you to specify the position. Will replace
existing variable with the same name if present.
}
\examples{
df <- data.frame(x = 1:5)
add_col(df, "y", runif(5))
add_col(df, "y", runif(5), where = 1)

add_col(df, "x", 5:1)
}
\seealso{
\code{\link[hadcol:insert_into]{hadcol::insert_into()}}

Other xyz: \code{\link{insert_into}}
}
\concept{xyz}
