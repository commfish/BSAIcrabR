# Load BSAI Observer Measure Pot Data

Load BSAI observer measure pot data and do data management routine

## Usage

``` r
load_crab_dump(path, stock, database_pull = F, clean = T)
```

## Arguments

- path:

  NULL. Path to data file if not pulling directly from database.

- stock:

  NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG,
  WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.

- database_pull:

  Default = FALSE. Do updated pull from database.

- clean:

  Default = TRUE. Do stock specific data cleaning.

## Value

Measure pot data time series by fishery.

## Examples

``` r
load_crab_dump("./data.csv", stock = "BBRKC")
#> Error in (function (path, write = FALSE, call = caller_env()) {    if (is.raw(path)) {        return(rawConnection(path, "rb"))    }    if (!is.character(path)) {        return(path)    }    if (is_url(path)) {        ext <- tolower(tools::file_ext(path))        if (ext %in% c("gz", "bz2", "xz", "zip")) {            cli::cli_abort("Remote compressed files must be handled upstream of this function.",                 .internal = TRUE)        }        if (requireNamespace("curl", quietly = TRUE)) {            con <- curl::curl(path)        }        else {            inform("`curl` package not installed, falling back to using `url()`")            con <- url(path)        }        return(con)    }    path <- enc2utf8(path)    if (write) {        path <- normalizePath_utf8(path, mustWork = FALSE)    }    else {        path <- check_path(path)    }    p <- split_path_ext(basename_utf8(path))    extension <- p$extension    formats <- archive_formats(extension)    while (is.null(formats) && nzchar(extension)) {        extension <- split_path_ext(extension)$extension        formats <- archive_formats(extension)    }    needs_archive <- !is.null(formats) && (write || extension !=         "zip")    if (needs_archive) {        reason <- glue("to {if (write) 'write' else 'read'} `.{p$extension}` files.")        rlang::check_installed("archive", reason = reason, call = call)        if (write) {            if (is.null(formats[[1]])) {                return(archive::file_write(path, filter = formats[[2]]))            }            return(archive::archive_write(path, p$path, format = formats[[1]],                 filter = formats[[2]]))        }        if (is.null(formats[[1]])) {            return(archive::file_read(path, filter = formats[[2]]))        }        return(archive::archive_read(path, format = formats[[1]],             filter = formats[[2]]))    }    if (!write) {        compression <- detect_compression(path)    }    else {        compression <- NA    }    if (is.na(compression)) {        compression <- tools::file_ext(path)    }    if (write && compression == "zip") {        cli::cli_abort(c("Can only read from, not write to, {.val .zip} files.",             i = "Install the {.pkg archive} package to write {.val .zip} files."),             call = call)    }    switch(compression, gz = gzfile(path), bz2 = bzfile(path),         xz = xzfile(path), zip = zipfile(path), if (!has_trailing_newline(path)) {            file(path)        } else {            path        })})("./data.csv"): ./data.csv does not exist in current working directory:
#> /home/runner/work/BSAIcrabR/BSAIcrabR/docs/reference.
```
