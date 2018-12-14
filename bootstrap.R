library(icesTAF)
taf.library(quiet=TRUE)

setwd("bootstrap")

mkdir(c("config", "data", "library", "software"))

## Process config
cp("initial/config", ".")

## Process data
datasets <- bibtex::read.bib("DATA.bib")
for(dat in datasets)
{
  if(grepl("^http", soft$source))
  {
    download(soft$source, dir="data")
  }
  else
  {
    if(dat$source == "file")
      dat$source <- file.path("initial/data", attr(dat,"key"))
    cp(dat$source, "data")
  }
}

## Process software
software <- bibtex::read.bib("SOFTWARE.bib")
for(soft in software)
{
  if(grepl("@", soft$source))
  {
    spec <- remotes::parse_repo_spec(soft$source)
    url <- paste0("https://api.github.com/repos/",
                  spec$username, "/", spec$repo, "/tarball/", spec$ref)
    targz <- paste0(spec$repo, "_", spec$ref, ".tar.gz")
    suppressWarnings(download(url, destfile=file.path("software", targz)))
    remotes::install_github(soft$source, upgrade=FALSE, force=TRUE)
  }
  else if(grepl("^http", soft$source))
  {
    download(soft$source, dir="software")
  }
  else
  {
    cp(soft$source, "software")
  }
}

## Remove empty folders
rmdir(c("config", "data", "library", "software"))
rmdir("library:", recursive=TRUE)  # this directory name can appear in Linux

setwd("..")
