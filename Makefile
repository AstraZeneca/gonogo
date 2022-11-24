#BEGIN CC BY-SA 4.0 - https://stackoverflow.com/a/51149133
NPROCS = $(shell grep -c 'processor' /proc/cpuinfo)
MAKEFLAGS += -j$(NPROCS)
#END

.PHONY: default
default: all

.PHONY: all
all: ci lint extra-all

.PHONY: install
install:
	Rscript install.R
.PHONY: ci
ci:
	Rscript ci.R

.PHONY: lint
lint:
	Rscript lint.R fail

.PHONY: check
check:
	Rscript check.R

.PHONY: build
build:
	Rscript build.R

.PHONY: clean
clean:
	rm extra/*.pdf
