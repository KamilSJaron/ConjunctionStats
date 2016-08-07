\name{getBoth}
\alias{getBoth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBoth(simtab, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simtab}{
%%     ~~Describe \code{simtab} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
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
function (simtab, s) 
{
    out <- tryCatch({
        binit <- 4 * (sqrt(8) * (sqrt(0.5)/sqrt(s)))^-1
        y = simtab$meanHI
        x = simtab$order
        fitModel = nls(y ~ a/(1 + exp(-b * (x - c))), start = list(a = 1, 
            b = binit, c = length(x)/2))
        res <- c(coef(fitModel)[2], coef(fitModel)[3])
        return(res)
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
