#!/usr/bin/env Rscript

## This script will update the documentation site using
## pkgdown and deploy it in gh-pages branch.

stopifnot(requireNamespace(("git2r")))
stopifnot(requireNamespace(("pkgdown")))
stopifnot(requireNamespace(("rmarkdown")))
stopifnot(requireNamespace(("withr")))

#stopifnot(git2r::libgit2_features()$ssh)

tmpdir <- tempfile(pattern = "tmp-gh-pages")
dir.create(tmpdir)
#repo <- git2r::clone(url = "git@github.com:Marlin-Na/TnT.git", path, branch = "gh-pages")
withr::with_dir(tmpdir, {
    system2(
        c("git","clone"),
        c("-b","gh-pages","git@github.com:Marlin-Na/TnT.git"),
        wait = TRUE
    )
})
path <- file.path(tmpdir, "TnT")
repo <- git2r::repository(path = path)


# Build site
pkgdown::build_site(path = path, examples = FALSE)

# Render some rmarkdown files
rmarkdown::render("notes.Rmd", output_dir = path)



git2r::add(repo, "*")
git2r::commit(repo, message = "Commit by `build.R`", all = TRUE, session = TRUE)
#git2r::push(repo)
withr::with_dir(path, {
    system2("git", "push", wait = TRUE)
})

cat("DONE!!")


