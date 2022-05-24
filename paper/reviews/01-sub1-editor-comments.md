The submission "mlr3spatiotempcv: Spatiotemporal resampling methods for machine learning in R" presents the R package mlr3spatiotempcv which builds on and extends package mlr3 to provide spatio-temporal resampling methods. The manuscript provides a comparison on some example of how results differ in case resampling methods are used which do not account for the spatio-temporal structure and contrasts it with the results obtained with the approach from the presented package. Also some discussion on suitable use is included.
The presentation of the implementation is currently already combined with some illustrative examples. This means that the implementation design is not discussed in sufficient detail. It would be good to better explain some of the design decisions, e.g., naming conventions, and also point out how extensibility of the package is guaranteed. It might also be of interest to a potential user to outline how the package could be extended by them or customized to their own needs.

A more general overview on the functionality provided and how it can be accessed and used would seem important in addition to the examples to facilitate the evaluation of the software contribution but also to guide readers to understand what the main functions with their main arguments are. This implies that Section 4 should be split into two sections where one focuses on the software design and presents the functionality provided before then having a separate section presenting the functions in more details as well as some examples of the use of the package illustrating the intended workflow to the reader. For this section it would also seem preferable to not only have a spatial, but a spatial-temporal example included illustrating the application of the package in this case. It would also be good to compare on a single example the different features and in this way provide readers with suitable example code and inform them about how to use the package in practice.
Before sending this into full review, we ask the authors to carefully improve the manuscript, replication code and the package. This would be important in order to ease review but also render this more appealing for the potential audience when published in JSS. In addition the submission needs to be proof-read and more care is needed to adhere to the JSS style guidelines.

## Specific comments
- The package version on CRAN is 1.0.1, the submitted package version is 1.0.0.9000. It would be preferable to have these versions in sync.
- The package seems to contain copy copied from other packages. The CRAN Repository Policy asks package authors to take care that authorship is not misrepresented.
- The replication code file should be adapted to remove parts which are knitr-specific for the dynamic document and maybe also avoid relying on some files being accessible with specific relative paths.
- The replication material cannot be successfully executed after installing the package mlr3spatiotempcv without enforcing the installation of the suggested packages. The error obtained is:

```
> rsmp_buffer = rsmp("spcv_buffer", theRange = 1000)Error: The following packages could not be loaded: blockCV
```

After intalling blockCV, the next error obtained is:
```
> autoplot(rsmp_buffer,+ size = 0.8, task = task, fold_id = 1) *+ ggplot2::scale_y_continuous(breaks = seq(-3.97, -4, -0.01)) *+ ggplot2::scale_x_continuous(breaks = seq(-79.06, -79.08, -0.01))Error: The following packages could not be loaded: ggtext
```

The "Writing R Extensions" manual recommends that a package should also be checked with the suggested packages not being available, e.g., by not having them available and setting `_R_CHECK_FORCE_SUGGESTS_=false`. In this case checking the package gives a number of errors.

The replication script should indicate that the package needs to be installed including the suggested packages in order to be able to smoothly run it and not having to install additional packages one by one in order to be able to successfully run the code. This seems rather cumbersome.
Assuming the replication code contains code demonstrating core functionality of the package, it is unclear why it cannot be run without having suggested packages available. Suggested packages should not be required for core functionality.

- It would seem that it would be beneficial to provide more methods. Usually it would be expected to have a print and a summary method, maybe also a plot method. Trying out summary() on the task object in the replication code gives:
```
> summary(task)Error in object[[i]] : wrong arguments for subsetting an environment
```

This is a rather surprising error with a not very helpful error message.
- The following check seems to rely on S3 class names raising doubts about suitability for other class systems in R with different inheritance:
  ```
  if (grepl("Repeated", class(resampling)[1])) {
  ```
- The manuscript abstract claims that various R packages exist implementing different spatiotemporal partitioning strategies. The enumeration of packages also lists package kmeans. However, there does not seem to be a package of this name on CRAN. The remaining manuscript also does not provide a formal reference for this package nor for all other packages mentioned (which would be required by the style guidelines of JSS).
- The manuscript text refers to a Table 4. However, the manuscript contains only a single table which is labeled Table 1.
- The style guidelines need to be better followed. The required mark-up for packages and software environments is not applied, the references to software package not included and the style file seems to have been tweaked to include more numberings. The list of footnotes looks awkward.
