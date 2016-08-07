\name{Fill2DSummary}
\alias{Fill2DSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fill2DSummary(sim, GradTable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
%%     ~~Describe \code{sim} here~~
}
  \item{GradTable}{
%%     ~~Describe \code{GradTable} here~~
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
function (sim, GradTable) 
{
    lsize <- sim[[1]]$DEME[sim[[1]]$DOWN == 0]
    for (i in 1:length(sim)) {
        B1s <- seq(max(sim[[i]]$order), max(sim[[i]]$order), 
            length = (lsize + 1))
        B0s <- seq(0, 0, length = (lsize + 1))
        for (h in 0:lsize) {
            selected <- seq(h, max(sim[[i]]$DEME), by = lsize + 
                1)
            subtable <- sim[[i]][sim[[i]]$DEME \%in\% selected, 
                ]
            if (any(subtable$meanHI == 1)) {
                B1 <- min(subtable$order[subtable$meanHI == 1]):max(subtable$order[subtable$meanHI == 
                  1])
                B1s[h + 1] <- max(c(B1[!(B1 \%in\% subtable$order[subtable$meanHI == 
                  1])], min(subtable$order[subtable$meanHI == 
                  1]) - 1))
            }
            if (any(subtable$meanHI == 0)) {
                B0 <- min(subtable$order[subtable$meanHI == 0]):max(subtable$order[subtable$meanHI == 
                  0])
                B0s[h + 1] <- min(c(B0[!(B0 \%in\% subtable$order[subtable$meanHI == 
                  0])]), max(subtable$order[subtable$meanHI == 
                  0]) + 1)
            }
        }
        minB0 <- min(B0s)
        maxB1 <- max(B1s)
        sim[[i]] <- sim[[i]][sim[[i]]$order \%in\% (minB0:maxB1), 
            ]
        ordervec <- sim[[i]]$order - minB0 + 1
        sim[[i]]$order = ordervec
    }
    return(sim)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
