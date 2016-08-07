\name{plotCF}
\alias{plotCF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotCF(FileName, GradTable, sim, i, ran = 5)
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
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{ran}{
%%     ~~Describe \code{ran} here~~
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
function (FileName, GradTable, sim, i, ran = 5) 
{
    pdf(FileName)
    palette <- brewer.pal(5, "RdYlBu")
    par(mfrow = c(1, 1))
    if (ran == 0) {
        plot(logit(meanHI) ~ order, data = sim[[i]], pch = 20)
    }
    else {
        plot(logit(meanHI) ~ order, data = sim[[i]], pch = 20, 
            xlim = c(GradTable[i, ]$center - ran, GradTable[i, 
                ]$center + ran))
    }
    marsx <- sim[[i]]$order[c((sim[[i]]$meanHI != 0) & (sim[[i]]$meanHI != 
        1))]
    marsy <- logit(sim[[i]]$meanHI)[c((sim[[i]]$meanHI != 0) & 
        (sim[[i]]$meanHI != 1))]
    marsmod <- earth(marsy ~ marsx)
    breakpoints <- unique(marsmod$cuts[-1])
    respond <- predict(marsmod, newdata = breakpoints)
    x <- seq(0.01, max(sim[[i]]$order), by = 0.01)
    y <- predict(marsmod, newdata = x)
    lines(x, y, col = palette[1])
    points(breakpoints, respond, col = palette[1], pch = 20)
    GradTableLine <- GradTable[i, ]
    binit <- 4 * (sqrt(8) * (sqrt(0.5)/sqrt(GradTableLine$s)))^-1
    y = sim[[i]]$meanHI
    x = sim[[i]]$order
    fitModel = nls(y ~ a/(1 + exp(-b * (x - c))), start = list(a = 1, 
        b = binit, c = length(x)/2))
    x <- seq(0.01, max(sim[[i]]$order), by = 0.01)
    y <- logit(sigmoid(summary(fitModel)$coefficients[, 1], x))
    lines(x, y, col = palette[2])
    y = sim[[i]]$meanHI[sim[[i]]$meanHI > 0.2 & sim[[i]]$meanHI < 
        0.8]
    x = sim[[i]]$order[sim[[i]]$meanHI > 0.2 & sim[[i]]$meanHI < 
        0.8]
    if (length(x) > 1) {
        mod8020 <- lm(y ~ x)
        x <- seq(min(x), max(x), by = 0.01)
        y <- logit(predict(mod8020, list(x = seq(min(x), max(x), 
            by = 0.01))))
        lines(x, y, col = palette[5])
    }
    else {
        y = sim[[i]]$meanHI[sim[[i]]$meanHI > 0.15 & sim[[i]]$meanHI < 
            0.85]
        x = sim[[i]]$order[sim[[i]]$meanHI > 0.15 & sim[[i]]$meanHI < 
            0.85]
        if (length(x) > 1) {
            mod8020 <- lm(y ~ x)
            x <- seq(min(x), max(x), by = 0.01)
            y <- logit(predict(mod8020, list(x = seq(min(x), 
                max(x), by = 0.01))))
            lines(x, y, col = palette[5])
        }
    }
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
