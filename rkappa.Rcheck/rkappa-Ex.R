pkgname <- "rkappa"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rkappa')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("project")
### * project

flush(stderr()); flush(stdout())

### Name: project
### Title: \it E. coli promoter transcription initiation model project
### Aliases: project
### Keywords: datasets

### ** Examples

data(project)
## maybe str(project) ; plot(project) ...



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
