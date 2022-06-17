# Submission 1 - editor reply

> A more general overview on the functionality provided and how it can be accessed and used would seem important in addition to the examples to facilitate the evaluation of the software contribution but also to guide readers to understand what the main functions with their main arguments are. This implies that Section 4 should be split into two sections where one focuses on the software design and presents the functionality provided before then having a separate section presenting the functions in more details as well as some examples of the use of the package illustrating the intended workflow to the reader. For this section it would also seem preferable to not only have a spatial, but a spatial-temporal example included illustrating the application of the package in this case. It would also be good to compare on a single example the different features and in this way provide readers with suitable example code and inform them about how to use the package in practice.

Thank you for the high-level feedback!
Transporting the package purpose to users is a very important part.
We have added a paragraph at the end of section 3 which explains the overall workflow in mlr3 of conducting a nested resampling - and where {mlr3spatiotempcv} comes into play in this process.
In addition, users can get a sense of the capabilities of the spatiotemporal methods in section 4.4 (Cross-validation for spatiotemporal data) and should be able to adapt the existing example easily to a spatiotemporal dataset thanks to the consistent building block structure of the mlr3 framework.
We are interested in getting more feedback on this topic and of how much important/interest the spatiotemporal methods are for users during a review of the manuscript.

For `mlr3spatiotempcv`, there are no "main functions" as there are for many other R packages.
Instead `mlr3spatiotempcv` acts as an extension package to the `mlr3` machine learning framework and provides resampling methods from various upstream sources.
Hence, we a dedicated section describing the main functions would not apply here in our view.
We list all the available resampling methods and their (subjective) categorization in Table 1, which is similar to the idea of presenting the "main functions" in our view.

Regarding more examples and practical use case tutorial: note that aside from this manuscript and the package examples, there is also the "mlr3book" and the "mlr3gallery" which aim to provide more helpful resources to users.
We have linked both resources at the appropriate places in a new version of the manuscript now.
In addition they are linked prominently in the package README or the package help page (?mlr3spatiotempcv::`mlr3spatiotempcv-package`).
Adding more uses cases to the manuscript would unnecessarily prolong the manuscript and not add much more added value to it.

> The package version on CRAN is 1.0.1, the submitted package version is 1.0.0.9000. It would be preferable to have these versions in sync.

The package is available on [GitHub](https://github.com/mlr-org/mlr3spatiotempcv) with all versions properly tagged, including version 1.0.0.9000.
It is common practice to bump the version after a CRAN release to indicate it's development status and distinguish it from a "stable" CRAN release.
At the point of release, the CRAN version was 1.0.0 and the submitted version 1.0.0.9000 with no further modifications to the version 1.0.0.
We could have also attached it with version 1.0.0 to avoid confusion - we apologize for the additional confusion caused from our side!
For the case at hand, a patch release was issued between the initial submission to JSS and this editor checks.
The NEWS file outlines the changes between the versions.
We believe that (at least) bug fixes should be acceptable during the review period if they do not change code functionality which has been shown in the submitted article.

For what its worth, one can also checkout a desired version from the GitHub source via the git tag directly using

```
git checkout tags/<tag>
```

Which would have been `git checkout tags/v1.0.0.9000` in this specific case.

For the resubmisson at hand, we have updated the package and package a new version on CRAN which will match the submitted version of the package.
This one can be checked out locally via

```
git clone https://github.com/mlr-org/mlr3spatiotempcv.git
cd mlr3spatiotempcv
git checkout tags/v2.0.0
```

> The package seems to contain copy copied from other packages. The CRAN Repository Policy asks package authors to take care that authorship is not misrepresented.

We apologize for this oversight.
We have added code comments to `ResamplingSpCVDisc` and `ResamplingSpCVTiles` highlighting code parts which were mainly copied from the {sperrorest} package.
While this is of course something that should always be done, we would like to note that both authors of the `sperrorest` package are also authors/contributors to {mlr3spatiotempcv} and/or this manuscript.

> The "Writing R Extensions" manual recommends that a package should also be checked with the suggested packages not being available, e.g., by not having them available and setting `_R_CHECK_FORCE_SUGGESTS_=false`. In this case checking the package gives a number of errors.

The env var `_R_CHECK_FORCE_SUGGESTS_` is not mentioned in https://cran.r-project.org/doc/manuals/r-release/R-exts.html (for R version 4.2.0, 2022-04-22) but only in https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Tools.
We think that instead of `_R_CHECK_FORCE_SUGGESTS_=false`, `_R_CHECK_FORCE_SUGGESTS_=TRUE` was meant on your side as the former does not result in an error if suggested packages are missing.
Instead we used `_R_CHECK_DEPENDS_ONLY_=TRUE` and could presumably reproduce the issues mentioned.
We have escaped more examples to not cause an error during `R CMD check`.
Note thought that `R CMD check --as-cran` uses `_R_CHECK_FORCE_SUGGESTS_=false` and that all package checks on CRAN are currently passing.
Currently the following R CMD check variations run without errors:

```r
rcmdcheck::rcmdcheck(args = c('--run-donttest','--as-cran', '--ignore-vignettes'), env = c("_R_CHECK_DEPENDS_ONLY_"=TRUE, "_R_CHECK_FORCE_SUGGESTS_"=TRUE))
rcmdcheck::rcmdcheck(args = c('--run-donttest','--as-cran', '--ignore-vignettes'), env = c("_R_CHECK_FORCE_SUGGESTS_"=TRUE))
```

Note that `--ignore-vignettes` is needed because otherwise an error would be thrown when aiming to build vignettes due to the absence of `rmarkdown`.
`rmarkdown` is commonly placed in SUGGESTS instead of IMPORTS as it is not needed for the core functionality of the package but only to render vignettes.

We'd be happy if requirements like these could be mentioned somewhere centrally on the JSS "information for authors" page to make it possible for authors to avoid such issues upfront submissions in the future.

> The replication script should indicate that the package needs to be installed including the suggested packages in order to be able to smoothly run it and not having to install additional packages one by one in order to be able to successfully run the code. This seems rather cumbersome. Assuming the replication code contains code demonstrating core functionality of the package, it is unclear why it cannot be run without having suggested packages available.

Thanks for the suggestions.
The decision to put the dependencies for individual methods into SUGGESTS rather than IMPORTS is a deliberate one: the average user usually does not need all available partitioning methods.
Most often, users decide for one method prior to executing code and are hence only interested in installing dependencies for this respective method.

The same applies for dependencies which provide visualization capabilities: many users might not be interested in making use of such but only want to use the partitioning functionality.
Subsequently we believe these should not be forced to install these plotting dependencies.
The article itself is also "special" in that case as it aims to showcase a large portion of the package's functionality which subsequently requires the installation of quite some "suggested" dependencies.

In addition, putting all upstream dependencies into IMPORTS would have side-effects with respect to the mlr3 framework as a whole as it would heavily increase the 1st order dependency chain for certain actions which load larger subparts of the framework.

That being said, we've added a conditional call to `install.packages()` to the replication script at the very beginning which now installs suggested packages of {mlr3spatiotempcv} which are not installed to make it easier for users and reviewers to execute the replication script.

> The replication code file should be adapted to remove parts which are knitr-specific for the dynamic document and maybe also avoid relying on some files being accessible with specific relative paths.

We have stripped all knitr-specific code parts from the replication script and only included the bare code (no figures, no other outputs from `knitr::spin()`).
We apologize for the oversight but would also appreciate if this could be outlined in more detail in the author information.

> It would seem that it would be beneficial to provide more methods. Usually it would be expected to have a print and a summary method, maybe also a plot method. Trying out summary() on the task object in the replication code gives:
> ```
> > summary(task)Error in object[[i]] : wrong arguments for subsetting an environment
> ```

A plot method for resampling objects is already included, see `autoplot()` and `plot()` methods listed in the function reference: https://mlr3spatiotempcv.mlr-org.com/reference/index.html.

In {mlr3} > 0.13.3.9000 a new minimal `summary()` method for Task objects is included so that `summary()` does not return an error anymore.
We might enhance `summary()` for task objects in the future.

> - The following check seems to rely on S3 class names raising doubts about suitability for other class systems in R with different inheritance:
>   ```
>   if (grepl("Repeated", class(resampling)[1])) {
>   ```

Thanks for the hint.
The code is within an S3 dispatch function for class `ResamplingSpCVDisc`, hence only objects of this S3 class will arrive arrive at this point.
Anyhow, we agree that the code is not really universally written and have rewritten is as

```r
  if (any(grepl("ResamplingRepeated", class(rsmp_autopl)))) {
```

> The manuscript abstract claims that various R packages exist implementing different spatiotemporal partitioning strategies. The enumeration of packages also lists package kmeans. However, there does not seem to be a package of this name on CRAN. The remaining manuscript also does not provide a formal reference for this package nor for all other packages mentioned (which would be required by the style guidelines of JSS).

Thanks for spotting this.
This was caused by a typo on our side, the referenced package is {skmeans}, not `kmeans`.
There are two other instances in the manuscript where the package was correctly referenced.
We have found that the reference for package {skmeans} was missing whereas all others were properly cited.

> - The manuscript text refers to a Table 4. However, the manuscript contains only a single table which is labeled Table 1.

Thanks for the information, we have corrected the reference.

> The style guidelines need to be better followed. The required mark-up for packages and software environments is not applied, the references to software package not included and the style file seems to have been tweaked to include more numberings. The list of footnotes looks awkward.

We apologize for the oversight and numbering tweaks.
We found two instances of package mentions not being formatted with `\pkg{}` (in a section header) and two instances of `\proglang{R}`.
Also we reverted the numbering tweaks and now solely rely on the available JSS `.cls` style file.
In addition we've applied markup to all figure captions for which we have left it out previously on purpose as the style guide stated "no additional formatting should be used for captions".
We are not sure if this change is desired, please advise if not.
