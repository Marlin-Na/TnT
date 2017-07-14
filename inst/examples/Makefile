
help:
	@echo "Targets:"
	@echo "... help"
	@echo "... build"

build:
	Rscript -e "rmarkdown::render_site()"

serve:
	cd _site; python3 -m http.server 1313

.PHONY: help build serve
