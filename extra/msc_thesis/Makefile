.PHONY: extra-all
extra-all: 18r_rate_figures_single.Rmd \
					 19r_policy_diagrams.Rmd \
					 21r_power_table.Rmd \
					 22r_powerless.Rmd \
					 23r_group_power.Rmd \
					 24r_univariate.Rmd \
					 25r_extra_unpowered.Rmd \
					 26r_case-study.Rmd \
					 27r_power_plot.Rmd \
					 28r_powerless_uni.Rmd \
					 29r_powerless_domains.Rmd \
					 30r_powerless_domains.Rmd \
					 34s_powerless_domains.Rmd \
					 38r_groupings_new.Rmd

%.pdf: %.Rmd
	Rscript -e "rmarkdown::render('$*.Rmd')"
