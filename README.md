# ConjunctionStats: an R package for parsing the output of Conjunction, the simulator of secondary contact.
** Developed by: Kamil S Jaron, Amina Echchiki **

```ConjunctionStats``` is a package for parsing the output of
[Conjunction](https://github.com/KamilSJaron/Conjunction) into R, to
allow statistical analysis.

# Installation

### Install via classical install

In terminal:

    git clone https://github.com/KamilSJaron/ConjunctionStats.git
    cd ./ConjunctionStats/

In R:
``` {r}
install.packages("./ConjunctionStats", repos = NULL, type="source")
```
### Install via GitHub

In R:

``` {r}
# install.packages("devtools")
library(devtools)
install_github("KamilSJaron/ConjunctionStats", build_vignettes=FALSE)
```

# Usage

### Load the package

``` {r}
library(ConjunctionStats)
```

### Access functions and documentation

``` {r}
# display function source code: FunctionName
# display function documentation: ?FunctionName
# example
ReadSetting
?ReadSetting
```

### List of useful functions

Here we only list all the functions, detailed descritions you can find in the manual pages of functions and plenty of examples you can find at repository [ConjunctionExamples](https://github.com/KamilSJaron/ConjunctionExamples)

**ConjunctionStats nomenclature**

* All non-internal functions are in CamelStyle
* `Read...` functions are reading files.
* `Fill...` functions are outputing an input object with some newly computed statictics
* `Get...` finctions are outputing a summary, statistics or reduced input object
* `Plot...` functions are plotting usually using paletes from package `Rcolorbrewer`

The core and the main purpose of the package is to provide an easy way how to **import Conjunction output to R** and compute some **statistics**.

**0D**

```{r}
to fill
```

**1D** 

``` {r}
setting <- ReadSetting(setting_file) # reads setting file, returns a dataframe of performed simulations
sims <- ReadSummary(out_file) # reads output file, returns a list of dataframes of demes of simulations
setting <- FillSetting(sims, setting) # add estimates of widths to setting frame using non-linear fitting of logistic function, depreciated for multilocus clines
setting <- FillSettingByHZAR(sims, setting) # add estimates of widths to setting frame using package HZAR
```

**2D**

```{r}
to fill
```

The second part of the package is used for plotting common hybrid zone graphics. See man pages of funcitons to see details.

```{r}
# high level plotting functions
PlotSim(...) # plot mean hybrid index, mean LD and average fitness across demes of one simulation
PlotStat(...) # make a dot plot with coloring of points according one of variables

# purely low level function
PlotAverages(...) # add averages to a plot, usually combined with PlotStat
```
