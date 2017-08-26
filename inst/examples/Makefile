
help:
	@echo "Targets:"
	@echo "... help"
	@echo "... build"
	@echo "... cp"
	@echo "... serve"

build:
	Rscript -e "rmarkdown::render_site()"

cp:
	if [ -d "../../gh-pages/examples" ]; then  rm -r ../../gh-pages/examples; fi
	mkdir ../../gh-pages/examples
	cp -r _site/* ../../gh-pages/examples

serve:
	cd _site; python3 -m http.server 1313

.PHONY: help build cp serve
