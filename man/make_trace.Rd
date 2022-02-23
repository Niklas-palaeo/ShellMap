\name{make_trace}
\alias{make_trace}
\title{make_trace()}
\usage{
make_trace(map,buffer,point_num,limit, resolution)
}
\description{
Plots 2D data and then lets you define a line consisting of a number of points, which will be used to make a transect, which is then plotted.

map = the dataframe or tibble that is in a format of x,y,data.

buffer = the distance from the line to the points that should be included in the transect. Units are in mm, default is 0.2 mm

point_num = the number of points within the line. Default is 2.

limit = the maximum value of the data, whcih is used to adjust the colour ramp. Default is 1.

resolution = the sampling resolution or distance between the individual points. Default is 100 micron.



}
\examples{
hello()
}
