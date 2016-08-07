\name{ReadSetting}
\alias{ReadSetting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadSetting(InputFile = "./setting.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{InputFile}{
%%     ~~Describe \code{InputFile} here~~
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
function (InputFile = "./setting.txt") 
{
    gradientTable <- data.frame(run = 1)
    summaryFile <- readLines(InputFile)
    for (l in summaryFile) {
        l <- strsplit(l, split = "#")[[1]][1]
        l <- strsplit(l, split = "=")[[1]]
        if (grepl("RECOMBINATIONrate", l[1])) {
            if (grepl("\\[", l[2])) {
                gradientTable <- merge(gradientTable, data.frame(r = as.double(strsplit(strsplit(strsplit(l[2], 
                  split = c("\\["))[[1]][2], split = "\\]")[[1]][1], 
                  split = ",")[[1]])))
            }
            else {
                gradientTable <- merge(data.frame(r = as.numeric(l[2])), 
                  gradientTable)
            }
        }
        if (grepl("SELECTION", l[1])) {
            if (grepl("\\[", l[2])) {
                gradientTable <- merge(gradientTable, data.frame(s = as.double(strsplit(strsplit(strsplit(l[2], 
                  split = c("\\["))[[1]][2], split = "\\]")[[1]][1], 
                  split = ",")[[1]])))
            }
            else {
                gradientTable <- merge(data.frame(s = as.numeric(l[2])), 
                  gradientTable)
            }
        }
        if (grepl("CHROM", l[1])) {
            if (grepl("\\[", l[2])) {
                gradientTable <- merge(gradientTable, data.frame(C = as.double(strsplit(strsplit(strsplit(l[2], 
                  split = c("\\["))[[1]][2], split = "\\]")[[1]][1], 
                  split = ",")[[1]])))
            }
            else {
                gradientTable <- merge(data.frame(C = as.numeric(l[2])), 
                  gradientTable)
            }
        }
        if (grepl("LOCI", l[1])) {
            if (grepl("\\[", l[2])) {
                gradientTable <- merge(gradientTable, data.frame(L = as.double(strsplit(strsplit(strsplit(l[2], 
                  split = c("\\["))[[1]][2], split = "\\]")[[1]][1], 
                  split = ",")[[1]])))
            }
            else {
                gradientTable <- merge(data.frame(L = as.numeric(l[2])), 
                  gradientTable)
            }
        }
        if (grepl("BETA", l[1])) {
            if (grepl("\\[", l[2])) {
                gradientTable <- merge(gradientTable, data.frame(b = as.double(strsplit(strsplit(strsplit(l[2], 
                  split = c("\\["))[[1]][2], split = "\\]")[[1]][1], 
                  split = ",")[[1]])))
            }
            else {
                gradientTable <- merge(data.frame(b = as.numeric(l[2])), 
                  gradientTable)
            }
        }
        if (grepl("NUMBERofSAVES", l[1])) {
            if (as.numeric(l[2]) > 1) {
                gradientTable <- merge(data.frame(Saves = as.numeric(l[2])), 
                  gradientTable)
            }
        }
        if (grepl("DEMEsize", l[1])) {
            if (grepl("\\[", l[2])) {
                gradientTable <- merge(gradientTable, data.frame(D = as.double(strsplit(strsplit(strsplit(l[2], 
                  split = c("\\["))[[1]][2], split = "\\]")[[1]][1], 
                  split = ",")[[1]])))
            }
            else {
                gradientTable <- merge(data.frame(D = as.numeric(l[2])), 
                  gradientTable)
            }
        }
    }
    gradientTable$run <- c()
    return(gradientTable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
