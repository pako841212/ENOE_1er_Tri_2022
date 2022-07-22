run:
	Rscript ENOE1_22.R

format:
	R -e "library(styler)" \
	-e "style_dir('src')"