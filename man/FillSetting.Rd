\name{FillSetting}
\alias{FillSetting}
\alias{Fill2DSetting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FillSetting(sim, GradTable)
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
    GradTable$slope <- 0
    GradTable$center <- 0
    GradTable$ss <- 0
    GradTable$total_demes <- 0
    GradTable$width_l <- 0
    GradTable$width_m <- 0
    GradTable$width_e <- 0
    for (i in 1:length(sim)) {
        GradTable$total_demes[i] <- nrow(sim[[i]])
        if (any(sim[[i]]$meanHI == 1)) {
            B1 <- min(sim[[i]]$order[sim[[i]]$meanHI == 1]):max(sim[[i]]$order[sim[[i]]$meanHI == 
                1])
            maxB1 <- max(c(B1[!(B1 \%in\% sim[[i]]$order[sim[[i]]$meanHI == 
                1])], min(sim[[i]]$order[sim[[i]]$meanHI == 1]) - 
                1))
        }
        else {
            maxB1 <- max(sim[[i]]$order)
        }
        if (any(sim[[i]]$meanHI == 0)) {
            B0 <- min(sim[[i]]$order[sim[[i]]$meanHI == 0]):max(sim[[i]]$order[sim[[i]]$meanHI == 
                0])
            minB0 <- min(c(B0[!(B0 \%in\% sim[[i]]$order[sim[[i]]$meanHI == 
                0])]), max(sim[[i]]$order[sim[[i]]$meanHI == 
                0]) + 1)
        }
        else {
            minB0 <- 1
        }
        sim[[i]] <- sim[[i]][sim[[i]]$order \%in\% (minB0:maxB1), 
            ]
        ordervec <- sim[[i]]$order - minB0 + 1
        sim[[i]]$order = ordervec
        GradTable$slope[i] <- getSlope(sim[[i]], GradTable[i, 
            ])
        GradTable$center[i] <- getCenter(sim[[i]], GradTable[i, 
            ])
        meanf <- sim[[i]]$meanf[order(sim[[i]]$order)]
        GradTable$ss[i] <- getSstar(meanf, 0.05, 1)
        GradTable$width_m[i] <- getWidthM(sim[[i]])
        GradTable$width_e[i] <- getWidthE(sim[[i]])
    }
    GradTable$width_l <- 4 * (GradTable$slope^-1)
    return(GradTable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
