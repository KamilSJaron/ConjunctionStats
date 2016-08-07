\name{plotWC}
\alias{plotWC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotWC(FileName, GradFileName, SimTabName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
%%     ~~Describe \code{FileName} here~~
}
  \item{GradFileName}{
%%     ~~Describe \code{GradFileName} here~~
}
  \item{SimTabName}{
%%     ~~Describe \code{SimTabName} here~~
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
function (FileName, GradFileName, SimTabName) 
{
    sim <- ReadSummary(SimTabName)
    GradTable <- ReadSetting(GradFileName)
    GradTable <- FillSetting(sim, GradTable)
    sim <- FillSummary(sim, GradTable$center)
    palette <- brewer.pal(5, "RdYlBu")
    palette[3] <- brewer.pal(6, "RdYlBu")[3]
    pdf(paste(FileName, ".pdf", sep = ""))
    x <- seq(0.01, 0.99, by = 0.01)
    CHs <- c(16, 8, 4, 2, 1)
    ylimt <- max(c(GradTable$width_l, GradTable$width_m, GradTable$width_e), 
        na.rm = T)
    plot(x, twidth(x), type = "l", xlim = c(min(GradTable$s), 
        max(GradTable$s)), ylim = c(0, ylimt), xlab = "s", ylab = "w", 
        cex.lab = 1.5, cex.axis = 1.5)
    for (i in 1:5) {
        if (all(GradTable$C == 1)) {
            points(width_l ~ s, subset = L == CHs[i], pch = 17, 
                cex = 0.6, data = GradTable, col = palette[i])
            lines(width_l ~ s, subset = L == CHs[i], data = GradTable, 
                col = palette[i])
            points(width_e ~ s, subset = L == CHs[i], pch = 15, 
                cex = 0.6, data = GradTable, col = palette[i])
            lines(width_e ~ s, subset = L == CHs[i], data = GradTable, 
                col = palette[i])
            points(width_m ~ s, subset = L == CHs[i], pch = 19, 
                cex = 0.6, data = GradTable, col = palette[i])
            lines(width_m ~ s, subset = L == CHs[i], data = GradTable, 
                col = palette[i])
        }
        else {
            points(width_l ~ s, subset = C == CHs[i], pch = 17, 
                cex = 0.6, data = GradTable, col = palette[i])
            lines(width_l ~ s, subset = C == CHs[i], data = GradTable, 
                col = palette[i])
            points(width_e ~ s, subset = C == CHs[i], pch = 15, 
                cex = 0.6, data = GradTable, col = palette[i])
            lines(width_e ~ s, subset = C == CHs[i], data = GradTable, 
                col = palette[i])
            points(width_m ~ s, subset = C == CHs[i], pch = 19, 
                cex = 0.6, data = GradTable, col = palette[i])
            lines(width_m ~ s, subset = C == CHs[i], data = GradTable, 
                col = palette[i])
        }
    }
    if (min(GradTable$s) < 0.2) {
        xleg <- 0.08
    }
    else {
        xleg <- 0
    }
    legend(0.585, ylimt, c("Logistic", "Endler", "MARS"), pch = c(17, 
        15, 19), cex = 1.5)
    legend(0.75 + xleg, ylimt, CHs, pch = 20, cex = 1.5, col = palette)
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
