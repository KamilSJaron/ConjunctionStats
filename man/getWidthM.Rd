\name{getWidthM}
\alias{getWidthM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getWidthM(simtab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simtab}{
%%     ~~Describe \code{simtab} here~~
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
function (simtab) 
{
    out <- tryCatch({
        relevant <- which(!(simtab$meanHI == 0 | simtab$meanHI == 
            1))
        marsmod <- earth(logit(meanHI) ~ order, data = simtab, 
            degree = 1, nk = 5, subset = relevant)
        bps <- unique(marsmod$cuts[-1])
        bps <- c(bps[1] - 1, unique(bps), bps[length(bps)] + 
            1)
        respond <- predict(marsmod, newdata = bps)
        slopes <- diff(respond)/diff(bps)
        return(4 * max(slopes)^(-1))
    }, error = function(cond) {
        return(NA)
    })
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
