\name{FillSummary}
\alias{FillSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FillSummary(sim, centers)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
%%     ~~Describe \code{sim} here~~
}
  \item{centers}{
%%     ~~Describe \code{centers} here~~
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
function (sim, centers) 
{
    for (i in 1:length(sim)) {
        B1max <- max(sim[[i]]$order)
        B0min <- 1
        if (any(sim[[i]]$meanHI == 1)) {
            B1 <- min(sim[[i]]$order[sim[[i]]$meanHI == 1]):max(sim[[i]]$order[sim[[i]]$meanHI == 
                1])
            B1max <- max(c(B1[!(B1 \%in\% sim[[i]]$order[sim[[i]]$meanHI == 
                1])], min(sim[[i]]$order[sim[[i]]$meanHI == 1]) - 
                1))
        }
        if (any(sim[[i]]$meanHI == 0)) {
            B0 <- min(sim[[i]]$order[sim[[i]]$meanHI == 0]):max(sim[[i]]$order[sim[[i]]$meanHI == 
                0])
            B0min <- min(c(B0[!(B0 \%in\% sim[[i]]$order[sim[[i]]$meanHI == 
                0])]), max(sim[[i]]$order[sim[[i]]$meanHI == 
                0]) + 1)
        }
        sim[[i]] <- sim[[i]][sim[[i]]$order \%in\% (B0min:B1max), 
            ]
        ordervec <- sim[[i]]$order - B0min + 1
        sim[[i]]$order = ordervec
        sim[[i]]$centered <- ordervec - centers[i]
    }
    return(sim)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
