#' Regression data of the usage of rental bikes in Washington D.C., USA
#'
#' This data set contains information on hourly bike sharing usage in Washington, D.C. for the years 2011-2012. The target variable is \code{count}, the total number of bikes lent out to users at a specific time.
#'
#' This data frame contains the following columns:
#' \describe{
#'  \item{\code{season}}{Season of the year}
#'  \item{\code{year}}{Year; 0=2011, 1=2012}
#'  \item{\code{month}}{Month of the year}
#'  \item{\code{holiday}}{If a day is a public holiday (y/n)}
#'  \item{\code{weekday}}{Day of the week}
#'  \item{\code{workingday}}{If a day is aworking day (y/n)}
#'  \item{\code{weather}}{Weather situation}
#'  \item{\code{temp}}{Temperature in degrees celsius}
#'  \item{\code{humidity}}{Humidity (relative)}
#'  \item{\code{windspeed}}{Windspeed in miles per hour}
#'  \item{\code{count}}{Total number of bikes lent out to users}  }
#'
#' @docType data
#'
#' @usage data(bikes)
#'
#' @source This is a subset of the original data, which can be found on the \href{https://www.openml.org/search?type=data&id=42712&sort=runs&status=active}{OpenML} database (ID = \code{42712}).
#'
#' @references Fanaee-T, Hadi, and Gama, Joao, "Event labeling combining ensemble detectors and background knowledge", Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg, doi:10.1007/s13748-013-0040-3.
#' @references Vanschoren, Joaquin, et al. "OpenML: networked science in machine learning." ACM SIGKDD Explorations Newsletter 15.2 (2014): 49-60.
"bikes"
