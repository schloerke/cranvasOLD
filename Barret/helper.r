#' Global Printing Check
#' checks to see if the print statements should be executed
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  if(if_bprint()) cat("Hello World")
if_bprint <- function()
{
  return(TRUE)
}

#' Print Item Information
#' prints the item name and structure information below it
#'
#' @param ... item to be quoted and then printed
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  dataRanges <- c(0,1,2,3)
#'  bprint(dataRanges)
bprint <- function(...) {
  if(if_bprint()) {
    cat(substitute(...),":\n")
    str(...)
  }
}


#' makes the data ranges
#' makes the data ranges with a small buffer
#'
#' @param dataColumn vector containing numeric elements
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_data_ranges(mtcars$disp)
make_data_ranges<-function(dataColumn){
  return(c(min(dataColumn)-0.25, max(dataColumn)+0.25))
#  return(range(dataColumn))
}



#' make a plot object
#' make a plot object
#'
#' @param windowRanges Ranges of the plotting window
#' @return Returns a Qt plot object
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_new_plot(make_window_ranges(c(0,1,2,3)))
make_new_plot <- function(windowRanges){
  new_plot(600,600, xrange=windowRanges[1:2], yrange=windowRanges[3:4])
}


#' make the window ranges
#' make the window ranges
#'
#' @param dataRanges ranges of the data so a buffer of space may be added
#' @param xlab if xlab is replaced with somthing other than null, it will be assumed that an axis label will be used
#' @param ylab if ylab is replaced with somthing other than null, it will be assumed that an axis label will be used
#' @return returns a vector of four variables containing xmin, xmax, ymin, ymax
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'  make_window_ranges(c(0,1,2,3))
make_window_ranges <- function(dataRanges, xlab=NULL, ylab=NULL)
{
  
  if(!is.null(ylab))
    xmin = dataRanges[1] - 0.2*diff(dataRanges[1:2])
  else
    xmin = dataRanges[1] - 0.13*diff(dataRanges[1:2])

  xmax = dataRanges[2]+0.1*diff(dataRanges[1:2])


  if(!is.null(xlab))
    ymin = dataRanges[3]-0.2*diff(dataRanges[3:4])
  else
    ymin = dataRanges[3]-0.13*diff(dataRanges[3:4])
    
  ymax = dataRanges[4]+0.1*diff(dataRanges[3:4])
  
  
  windowRanges <- c(
      xmin,
      xmax,
      ymin,
      ymax
  )
  
  
  if(if_bprint())
    cat("Window Range: x=c(", windowRanges[1],", ",windowRanges[2],")  y=(",windowRanges[3],", ",windowRanges[4],")\n")

  windowRanges  
}


add_title <- function(plotObj, dataRanges, title)
{
  plotObj$add_layer(
    text(
      text=title,
      left=dataRanges[1]+ 0.5*diff(dataRanges[1:2]),
      bottom=dataRanges[4] + 0.05*diff(dataRanges[3:4]), 
      stroke="black",
      valign="top"
    )
  )

  
}