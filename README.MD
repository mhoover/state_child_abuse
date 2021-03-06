# README
## Status
Complete

## Introduction
I once made an off-handed comment about child abuse being way more prevalent in the U.S. south then elsewhere in the country. I was called out on it and I was mistaken. And I know this because I gathered some data the next morning to look into whether my assertion was the wild hyperbole that I probably intended it to be, but still shouldn't have been flippant enough to say.

The tragedy is that child abuse does happen and that means it is happening too much. I'm sorry for my comment (I apologized to those present when I sent them my work) and for anyone interested in following organizations fighting child abuse, here are a couple:
- [National Children's Alliance](http://www.nationalchildrensalliance.org)
- [Court Appointed Special Advocates for Children (CASA)](http://www.casaforchildren.org)
- [Safe Horizon](http://www.safehorizon.org)

And, not per se a child abuse advocacy organization, but a great organization working on behalf of all children, [The Children's Defense Fund](http://www.childrensdefense.org).

## What's here
This repo includes a couple of scripts to grab and clean data from the U.S. Census and elsewhere, then run some very simple (purposely done so... more could certainly be done and one should feel free to try!) regressions and some maps about state-level child abuse.

There is also an R package I wrote to handle some of the data import and formatting work. This will be needed to be able to run the scripts as written. To install, do the following:

```r
library(devtools)
install_github('mhoover/state_child_abuse/FormatFunctions')
```

## Contact
If you've got questions, give me a shout at matthew.a.hoover at gmail.com.
