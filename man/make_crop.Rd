\name{make_crop}
\alias{make_crop}
\title{make_crop()}
\usage{
make_crop(map, limit)
}
\description{
Crops the point data to the preferred size. After you run the command, you need to click twice onto the plot to define a rectangular area. The positions you click on will be the opposing corners of the rectangle.

map = the dataframe or tibble that is in a format of x,y,data.

limit = the maximum value of the data, whcih is used to adjust the colour ramp. Default is 1.

}
\examples{
make_crop(map)
}
