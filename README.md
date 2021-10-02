# Contents:
-   [Abstract](#Abstract)
-   [Oxforddown](#Oxforddown)
-   [Docker users](#Docker-users)

# Abstract

This thesis explores two questions: does the way individuals seek advice produce echo chamber-like networks; and is the well-established phenomenon of egocentric discounting explicable as a rational process?
Both parts are presented within a framework of advice as information transfer; the implications for wider interpretations of advice are discussed in the conclusion.
Both parts are investigated with a mixture of computational simulations and behavioural experiments.

For the first question, behavioural experiments implementing a Judge-Advisor System with a perceptual decision-making task and a date estimation task are used to characterise people's propensity to use agreement as a signal of advice quality in the absence of feedback.
These experiments provide moderate evidence suggesting that people do do this, and that experience of agreement in the absence of feedback increases their trust in advisors.
Agent-based computational simulations take the results of the behavioural experiments and simulate their effects on trust ratings between agents.
The simulations indicate that including the kind of heterogeneity seen in the participants in the behavioural experiments slows down the formation of echo-chambers and limits the extent of polarisation.

In the second part, I argue that egocentric discounting deviates from a normative model of advice-taking because it is a rational response to concerns that always accompany advice: that the advice might be deliberately misleading, lazily researched, or misunderstood.
Evolutionary computational simulations of advice-taking illustrate that when any of these circumstances might be true, egocentric discounting emerges as an adaptive response.
Behavioural experiments using a date estimation task within a Judge-Advisor System test whether people respond adaptively to alterations in the circumstances explored in the evolutionary simulations.
These experiments show that people respond flexibly to changes in the probability that their advisor will attempt to mislead them.
Experiments attempting to explore people's ability to flexibly respond to acquiring information about an advisor's confidence calibration were inconclusive.

# Oxforddown

This thesis is written in RMarkdown using Ulrik Lyngs' [oxforddown](https://github.com/ulyngs/oxforddown/) template for Oxford University.
Oxforddown uses the [bookdown](https://bookdown.org) R package together with the [OxThesis LaTeX template](https://github.com/mcmanigle/OxThesis), plus lots of inspiration from [thesisdown](https://github.com/ismayc/thesisdown).

## Requirements

The thesis was developed using:
-   [R](https://cran.rstudio.com) version 4.1.0
-   [RStudio version 1.4.1717](https://www.rstudio.com/products/rstudio/download/#download)

-   R packages managed using `renv::`

-   LaTeX via [TinyTeX](https://yihui.name/tinytex/)

# Docker users

If you have used the Docker image that contains the thesis code and environment, you can build the thesis from the command line using R.
You enter the container in the relevant directory `/oxforddown`.
The container has very few commands available (you can install more with `apt-get install [software]`), but you can browser the files with `ls` and `cat`.
To create the thesis from the ingredients, first we remove the existing docs folder:

``` bash
rm -rf docs
```

Next, we tell R to run the `render_book` function from the `bookdown::` package:

``` bash
Rscript -e 'bookdown::render_book("index.Rmd")'
```

If we want R to do more of the calculation work, we can tell it to avoid simply downloading data and try to recreate it itself. 
This does not necessarily work for all parts of the thesis, but it does work for some of the modelling stuff.
In the command below we replace `[x]` with some number greater than 0. 
Using 1 will make the processing time be hours as opposed to minutes; using 2 or greater will result in a processing time of several hours and the process may crash due to lack of RAM.

``` bash
Rscript -e 'options(ESM.recalculate = [x]); bookdown::render_book("index.Rmd")'
```

