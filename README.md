---
output: html_document
---
# ConjunctionStats: an R package for parsing the output of Conjunction, the simulator of secondary contact.
##### Kamil S Jaron, Amina Echchiki

```ConjunctionStats``` is a package for parsing the output of 
Conjunction (https://github.com/KamilSJaron/Conjunction) into R, to 
allow statistical analysis.

#TODO: description of Conjunction and usage of ConjunctionStats


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
