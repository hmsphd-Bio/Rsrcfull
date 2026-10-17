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



#------------------------------------------------
#' Radians_to_Degrees
#' A simple everday-use function to convert radians to degrees.
#'
#' @param r a numeric vector of angles(in radians) with a length of atleast 1.
#' @param round a numeric value between 1 and 5.

#' @export
rad2deg <- function(r,round = NULL) {
  if (!length(r)) stop("The vector is empty, please provide a valid vector of angles(in radians), with length > 0.\n")

  rd = r*(180/pi)

  if (!is.null(round)) {
    if (round < 1 || round > 5) stop("The output number can only be rounded from 1 to 5 decimal digits.")
  rd_rounded = round(rd,round)
  return(rd_rounded)} else {return(rd)}

}


#------------------------------------------------
#' Degrees_to_Radians
#'
#' A simple everday-use function to convert degrees to radians.
#'
#' @param d a numeric vector of angles(in degrees) with a length of atleast 1.
#' @param round a numeric value between 1 and 5.
#' @param show_pi (logical) =TRUE will display the radians as a multiple of pi; =FALSE will display standard numeric value of angle(s) in radians.
#' @export
deg2rad <- function(d,round = NULL,show_pi = FALSE) {
  if (!length(d)) stop("The vector is empty, please provide a valid vector of angles(in degrees), with length > 0.\n")

  rr = d*(pi/180)

  if (!is.null(round)) {
    if (round < 1 || round > 5) stop("The output number can only be rounded from 1 to 5 decimal digits.")
    rr_rounded = round(rr,round)
    return(rr_rounded)} else {if(show_pi) { return(paste0(round(rr/pi,2),intToUtf8(960))) } else{return(d*(pi/180))}}

}


#------------------------------------------------
#' Geometric Mean
#' Calculates the geometric mean of a vector of numeric values.
#'
#' @param x a numeric vector of length >= 1.Geometric mean does not accept negative values or zero.
#' @export
geometric_mean <- function(x) {
  if (!length(x)) stop("The vector is empty, please provide a valid vector of numeric values with length > 0.\n")

  if (any(x <= 0)) {
    index_to_be_excluded = which(x<=0)
    warning(paste0("The provided vector has negative or zero values at index -> ",index_to_be_excluded,collapse,".These were NOT included in calculating the Geometric mean.\n  "))
  }

  if (anyNA(x)) {warning("The provided vector has NAs; these were removed before calculating the geometric mean.")}

  filtered_nas = na.omit(x)
  filtered_nas_neg = filtered_nas[filtered_nas > 0]

  return(prod(filtered_nas_neg)**(1/length(filtered_nas_neg)))


}



#--------------------------------------------------------------

#' Harmonic Mean
#'
#' Calculates the harmonic mean of a numeric vector.
#'
#' @param x A numeric vector.
#' @param na.rm A logical value indicating whether `NA` values should be stripped before the computation proceeds. Defaults to `FALSE`.
#' @return The harmonic mean as a numeric value.
#' @export
#'
#' @examples
#' harmonic_mean(c(1, 2, 4))
#' harmonic_mean(c(10, 20, 30), na.rm = TRUE)

harmonic_mean <- function(x, na.rm = FALSE) {
  # 1. Check if input is numeric
  if (!is.numeric(x)) {
    stop("Argument 'x' must be a numeric vector.")
  }

  # 2. Handle missing values (NA) like base::mean() does
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA_real_)
  }

  # 3. Check if vector is empty after cleaning
  if (length(x) == 0) {
    return(NaN)
  }

  # 4. Check for zeros or negative values (using the <= 0 check we discussed!)
  if (any(x <= 0)) {
    stop("Harmonic mean is not defined for zero or negative values.")
  }

  # 5. Calculate harmonic mean: n / sum(1 / x)
  n <- length(x)
  h_mean <- n / sum(1 / x)

  return(h_mean)
}
