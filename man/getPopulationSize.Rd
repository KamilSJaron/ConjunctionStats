\name{getPopulationSize}
\alias{getPopulationSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPopulationSize(LogFile, generation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LogFile}{
%%     ~~Describe \code{LogFile} here~~
}
  \item{generation}{
%%     ~~Describe \code{generation} here~~
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
function (LogFile, generation) 
{
    Js <- c()
    counter = 0
    for (l in LogFile) {
        counter = counter + 1
        if ("Selection" \%in\% (strsplit(l, split = ":")[[1]])) {
            counter <- 0
        }
        if (counter == generation) {
            if (generation == 0) {
                Js <- c(Js, as.numeric(strsplit(ll, split = " ")[[1]][3]))
            }
            else {
                Js <- c(Js, as.numeric(strsplit(l, split = " ")[[1]][3]))
            }
        }
        ll = l
    }
    return(Js)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
