\name{getWidthE}
\alias{getWidthE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getWidthE(simtab)
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
    y = logit(simtab$meanHI[simtab$meanHI > 0.2 & simtab$meanHI < 
        0.8])
    x = simtab$order[simtab$meanHI > 0.2 & simtab$meanHI < 0.8]
    if (length(x) > 1) {
        mod8020 <- lm(y ~ x)
        return(4/mod8020$coefficients[2])
    }
    else {
        y = logit(simtab$meanHI[simtab$meanHI > 0.15 & simtab$meanHI < 
            0.85])
        x = simtab$order[simtab$meanHI > 0.15 & simtab$meanHI < 
            0.85]
        if (length(x) > 1) {
            mod8020 <- lm(y ~ x)
            return(4/mod8020$coefficients[2])
        }
        else {
            return(NA)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
