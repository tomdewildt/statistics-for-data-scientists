.PHONY: init run lint
.DEFAULT_GOAL := help

NAMESPACE := tomdewildt
NAME := statistics-for-data-scientists

TYPE := chapters
CHAPTER := 1

help: ## Show this help
	@echo "${NAMESPACE}/${NAME}"
	@echo
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

##

init: ## Initialize the environment
	Rscript -e "renv::restore()"

##

run: ## Run the script
	Rscript src/${TYPE}/chapter_${CHAPTER}.R

##

lint: ## Run lint
	Rscript -e "lintr::lint_dir('src')"
