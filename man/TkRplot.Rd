\name{TkRplot}
\title{Tk Rplot}
\description{
  Placing R graphics in a Tk image.
}
\alias{tkrplot}
\alias{tkrreplot}
\alias{tkpersp}
\usage{
tkrplot(parent, fun, hscale=1, vscale=1)
tkrreplot(lab, fun = lab$fun, hscale=lab$hscale, vscale=lab$vscale)
tkpersp(x,y,z, theta = 30,phi = 30,expand = 0.5, r = sqrt(3), ...)
}
\arguments{
  \item{parent}{parent of widget window}
  \item{fun}{function of no arguments that creates the plot}
  \item{lab}{a Tk Rplot label widget}
  \item{hscale}{horizontal scale factor for image size}
  \item{vscale}{vertical scale factor for image size}
  \item{x}{as for \code{persp}}
  \item{y}{as for \code{persp}}
  \item{z}{as for \code{persp}}
  \item{theta}{as for \code{persp}}
  \item{phi}{as for \code{persp}}
  \item{expand}{as for \code{persp}}
  \item{r}{as for \code{persp}}
  \item{...}{additional arguments for \code{persp}}
}
\details{
  The function \code{tkrplot} creates and returns a Tk label widget
  containing a Tk image of type Rplot.  For now the size is hard-wired.
  The plot is created by calling \code{fun} with a special device used
  create the image.

  The function \code{tkrreplot} calls \code{fun} to place a new plot
  in the Rplot widget \code{lab}.

  \code{tkpersp} is called like \code{persp} but produces a plot in
  which some of the parameters of \code{persp} are controlled
  graphically.
}

\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- tktoplevel()
bb<-1
img <-tkrplot(tt, function() plot(1:20,(1:20)^bb))
f<-function(...) {
    b <- as.numeric(tclvalue("bb"))
    if (b != bb) {
        bb <<- b
        tkrreplot(img)
    }
}
s <- tkscale(tt, command=f, from=0.05, to=2.00, variable="bb",
             showvalue=FALSE, resolution=0.05, orient="horiz")
tkpack(img,s)
}
}
\keyword{ misc }
