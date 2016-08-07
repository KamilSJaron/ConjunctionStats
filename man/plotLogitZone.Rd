\name{plotLogitZone}
\alias{plotLogitZone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotLogitZone(FileName, GradTable, sim, sel, ran = 0, type = "pdf", leg = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
%%     ~~Describe \code{FileName} here~~
}
  \item{GradTable}{
%%     ~~Describe \code{GradTable} here~~
}
  \item{sim}{
%%     ~~Describe \code{sim} here~~
}
  \item{sel}{
%%     ~~Describe \code{sel} here~~
}
  \item{ran}{
%%     ~~Describe \code{ran} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{leg}{
%%     ~~Describe \code{leg} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (FileName, GradTable, sim, sel, ran = 0, type = "pdf", 
    leg = T) 
{
    if (type == "pdf") {
        pdf(FileName)
    }
    else {
        if (type == "png") {
            png(FileName)
        }
        else {
            return("Error: unknown type (pdf or png)")
        }
    }
    picked <- which(GradTable$s == round(sel, 2))
    x <- logit(sim[[picked[length(picked)]]]$meanHI[order(sim[[picked[length(picked)]]]$order)])
    y <- sim[[picked[length(picked)]]]$centered[order(sim[[picked[length(picked)]]]$order)]
    palette <- rev(brewer.pal(8, "RdYlBu"))[c(-3, -4)]
    if (ran == 0) {
        plot(x ~ y, type = "l", col = palette[length(picked)], 
            xlab = "centralized indices", ylab = "logit(average hybrid index)", 
            lwd = 2, cex.lab = 1.5, cex.axis = 1.5)
    }
    else {
        plot(x ~ y, type = "l", col = palette[length(picked)], 
            xlab = "centralized indices", ylab = "logit(average hybrid index)", 
            xlim = c(-ran, ran), lwd = 2, cex.lab = 1.5, cex.axis = 1.5)
    }
    for (i in 1:(length(picked))) {
        x <- logit(sim[[picked[i]]]$meanHI[order(sim[[picked[i]]]$order)])
        y <- sim[[picked[i]]]$centered[order(sim[[picked[i]]]$order)]
        lines(x ~ y, col = palette[i], lwd = 2)
    }
    if (leg == T) {
        legend(min(y) + 1, 10, legend = c(1, 2, 4, 8, 16), col = palette, 
            lty = 1, lwd = 2, title = "Loci", cex = 1.5)
    }
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
