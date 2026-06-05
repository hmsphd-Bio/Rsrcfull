### Note: DO NOT REMOVE OR MODIFY THE 'HASH+SINGLE QUOTE' SIGNS BELOW.THEY ARE NOT COMMENTS
### BUT ARE ESSENTIALLY REQUIRED BY THE `ROXYGEN2` PACKAGE TO PARSE THIS DOCUMENT AND UPDATE THE METADATA INTO ASSOCIATED FILES.
###------------------------------------------------------------------------------


#' Quick Head and Tail Preview
#'
#' Prints both the initial and trailing rows of a dataframe with custom headers.
#'
#' @param x A dataframe.
#' @param n Number of rows to print for both head and tail (default is 5).
#' @export
ht <- function(x,n=5) {

  if (is.data.frame(x) != TRUE) {
    stop("The input must be a dataframe!")
  }
  else {
    cat("\t---Initial",n,"entries---\n")
    print(head(x,n))
    cat("\t---last",n,"entries---\n")
    print(tail(x,n))
  }
}

###-----------------------------------------------------------------------------

#' Count Missing Values Column-Wise
#'
#' Scans a dataframe and prints the count of NA values for columns that contain them,
#' preserving the original column numbers.
#'
#' @param x A dataframe.
#' @export
countNA <- function(x) {
  total_NAs <- colSums(is.na(x))
  positive <- total_NAs[total_NAs > 0]

  if (!is.data.frame(x)) {
    stop("Input (x) must be a data frame!")
  }

  if (!anyNA(x)) {
    cat("No NAs were found")
    return(invisible(NULL))
    }

  else{
        cat("\n",length(positive),"out of",length(total_NAs),"columns have NAs.\n")
        cat("\n\t-:COLUMN-WISE COUNTS:-\n")
        cat("-----------------------------------------\n")

        for (i in seq_along(total_NAs)) {
          if (total_NAs[i] > 0 ){
          cat("\t",paste0("{",i,"}"),names(total_NAs)[i],"-->",total_NAs[i],"\n")
          }
        }
        cat("-----------------------------------------\n")
        }
      }

###-----------------------------------------------------------------------------

#' Change the Number and layout of Sub-plots in Plots Pane.
#'
#' Uses the par(mfrow=c(rows,columns)) method to change the number and layout of sub-plots
#' that appear in the plots pane.
#'
#'@param rows Number of vertical lines of the grid in which plots are to be arranged.
#'@param columns Number of horizontal lines of the grid in which plots are to be arranged.
#'@export
multiplots <- function(rows,columns) {
  if (!is.numeric(rows) || !is.numeric(columns)) stop("Both 'rows' and 'columns' must be numeric!")

  invisible(par(mfrow =c(rows,columns)))
  cat("Plots layout changed!.You can now plot",rows*columns,"plots in a single window.\n")
}
###------------------------------------------------------------------------------

#' Calculates the cube root of the desired number or a vector of numbers.
#'
#'@returns Cube root of the given number, rounded to two decimal places.
#'@param x A numeric or a vector of numeric integers or floats.
#'@export
cqbrt <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a single number or a Vector of numbers.")
  }
  cube_root <- round(x^(1/3),2)
  return(cube_root)
}

#------------------------------------------------
#' Make a line plot using base R graphics.
#'
#' @param x a numeric vector
#' @param title Title of the plot
#' @param xlim limits of the X-axis
#' @param ylm Limits of the Y-axis
#' @param ylab Label of the Y-axis
#' @param xlab Label of the X-axis
#' @param h_line_at Draws a horizontal dotted line at the specified point on y-axis
#' @export
plotline <- function(x,title = NULL,color="red",xlim=NULL,ylim=NULL,ylab =NULL,xlab =NULL, h_line_at = NULL,v_line_at = NULL) {
  if (is.vector(x) && is.numeric(x)){
  plot_args = list(x = as.numeric(x),type = "l",col = as.character(color),lwd=1.5)

  label = deparse(substitute(x))

    if (!is.null(title)) plot_args$main <- title

    if (!is.null(xlim)) plot_args$xlim <- xlim
    if (!is.null(ylim)) plot_args$ylim <- ylim

    plot_args$xlab <-  if (!is.null(xlab))  xlab else ""
    plot_args$ylab <-  if (!is.null(ylab))  ylab else label



  do.call(plot,plot_args)

  if (!is.null(h_line_at)) abline(h=h_line_at,lty= 2,col="gray")
  if (!is.null(v_line_at)) abline(v=v_line_at,lty=2,col= "gray")

      } else stop("x must be a numeric vector.\n")

}
