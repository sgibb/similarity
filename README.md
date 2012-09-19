# Similarity measurement for binary matrices.


## Install

```R
install.packages("devtools")
library("devtools")
install_github("similarity", "sgibb")
```

## Features

### Asymmetric Inidces
- Jaccard
- Soerensen/Dice

### Symmetric Inidces
- Simple Matching
- Rogers-Tanimoto

## Example

```R
library("similarity")

a <- matrix(c(1, 1, 1,
              0, 0, 1,
              1, 1, 0,
              1, 0, 1),
            nrow=4, ncol=3, byrow=T)

similarity(a, "soerensen")
# 1         2         3
# 2 0.5000000                    
# 3 0.8000000 0.0000000          
# 4 0.8000000 0.6666667 0.5000000

similarity(a, "simplematching")
#           1         2         3
# 2 0.3333333                    
# 3 0.6666667 0.0000000          
# 4 0.6666667 0.6666667 0.3333333

```
