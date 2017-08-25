#!/usr/bin/env fish

if not test (count $argv) -eq 1
    if test (count $argv) -eq 0
        set_color yellow
    else
        set_color red
    end
    echo "Options:"
    echo "    doc       -- Run roxygen"
    echo "    check     -- Check package"
    echo "    biocheck  -- Bioc Check"
    echo "    site      -- Build documentation site"
    echo "    example   -- Build example site"
    echo "    serve     -- Serve site"
    exit 0
end

set -l pkgdir (dirname (status -f))

if not test -d $pkgdir/gh-pages
    set_color red
    echo gh-pages dir does not exist, nothing will been done.
    exit 1
end

set_color yellow

switch $argv
    case doc
        echo ------------  Start running roxygen  ----------------------
        Rscript -e "devtools::document('$pkgdir')"
        #and Rscript -e "devtools::install('$pkgdir')"
        #and R CMD INSTALL $pkgdir --with-keep.source --no-multiarch
        or exit 1
    case check
        echo ------------  Start checking package  ---------------------
        Rscript -e "devtools::check('$pkgdir')"
        or exit 1
    case biocheck
        echo ------------  Start Bioc Check  ---------------------------
        Rscript -e "BiocCheck::BiocCheck('$pkgdir')"
        or exit 1
    case site
        echo ------------  Start building documentation site  ----------
        Rscript -e "pkgdown::build_site(pkg = '$pkgdir', path = '$pkgdir/gh-pages', examples = TRUE)"
        or exit 1
    case example
        echo ------------  Start building example site  ----------------
        cd $pkgdir/inst/examples/
        make build; and make cp
        or exit 1
    case serve
        cd $pkgdir/gh-pages
        and python3 -m http.server 1313
    case '*'
        eval (status -f) a b c
        exit 1
end

set_color green
echo ------------  Done  ---------------------------------------

