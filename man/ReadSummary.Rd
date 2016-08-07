\name{ReadSummary}
\alias{ReadSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadSummary(nameIn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nameIn}{
%%     ~~Describe \code{nameIn} here~~
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
function (nameIn) 
{
    summaryFile <- readLines(nameIn)
    i = 0
    sim <- list()
    for (l in summaryFile) {
        if ("EDGE" \%in\% strsplit(l, split = " ")[[1]]) {
            if (i > 0) {
                names(tableTemp) <- ColNames
                tableTemp$order <- 0
                tableTemp$order[which(!(tableTemp$LEFT \%in\% tableTemp$DEME))] <- 1
                nextRight <- tableTemp$RIGHT[tableTemp$order == 
                  1]
                for (j in 2:nrow(tableTemp)) {
                  tableTemp$order[tableTemp$DEME == nextRight] <- j
                  nextRight <- tableTemp$RIGHT[tableTemp$order == 
                    j]
                }
                sim[[i]] <- tableTemp
            }
            i = i + 1
            tableTemp <- data.frame()
        }
        else {
            newl <- strsplit(l, split = " ")[[1]][strsplit(l, 
                split = " ")[[1]] != ""]
            if (newl[1] == "DEME") {
                ColNames <- newl
                if (ColNames[4] == "mean") {
                  ColNames <- ColNames[-4]
                  ColNames[4] <- "meanf"
                }
                if (ColNames[6] == "mean") {
                  ColNames <- ColNames[-6]
                  ColNames[6] <- "meanf"
                }
            }
            newl <- strsplit(l, split = " ")[[1]][strsplit(l, 
                split = " ")[[1]] != ""]
            if (grepl("[[:digit:]]", newl[1])) {
                newl[1] <- gsub("[^0-9]", "", newl[1])
                tableTemp <- rbind(tableTemp, as.numeric(newl))
            }
        }
    }
    names(tableTemp) <- ColNames
    tableTemp$order <- 0
    tableTemp$order[which(!(tableTemp$LEFT \%in\% tableTemp$DEME))] <- 1
    nextRight <- tableTemp$RIGHT[tableTemp$order == 1]
    for (j in 2:nrow(tableTemp)) {
        tableTemp$order[tableTemp$DEME == nextRight] <- j
        nextRight <- tableTemp$RIGHT[tableTemp$order == j]
    }
    sim[[i]] <- tableTemp
    return(sim)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
