#!/usr/bin/env fish

if not test (count $argv) -eq 1
    echo "Options:"
    echo "    site      -- Build documentation site"
    echo "    example   -- Build example site"
    echo "    serve     -- Serve site"
    exit 1
end

set -l pkgdir (dirname (status -f))

if not test -d $pkgdir/gh-pages
    echo gh-pages dir does not exist, nothing will been done.
    exit 1
end

switch $argv
    case site
        echo Start building documentation site......
        Rscript -e "pkgdown::build_site(pkg = '$pkgdir', path = '$pkgdir/gh-pages', examples = TRUE)"
        and echo Done......
    case example
        echo Start building example site......
        cd $pkgdir/inst/examples/
        make build; and make cp; and echo Done......
    case serve
        cd $pkgdir/gh-pages
        python3 -m http.server 1313
end

