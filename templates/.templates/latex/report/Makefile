BUILD_DIR = build
FIGURES_DIR = fig
FIGURES := $(wildcard "$(FIGURES_DIR)/*.pdf")
RELEASE = report-$(VERSION)
SHELL = /bin/sh
VERSION = v0.0.0

TARGETS := $(addprefix $(BUILD_DIR)/, report.pdf)
COMPILE_TEX := latexmk -output-directory=$(BUILD_DIR)

.SUFFIXES: .bib .pdf .tex
.PHONY: all create_build_dir clean check compile_pdf

all: $(TARGETS)

$(BUILD_DIR)/%.pdf: %.tex
	@$(COMPILE_TEX) $<

$(TARGETS): | $(BUILD_DIR)

watch: | $(BUILD_DIR)
	@$(COMPILE_TEX) -pvc $(wildcard "*.tex")

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

check: | $(BUILD_DIR)
	@find . -name *.tex -exec chktex {} \; | tee "$(BUILD_DIR)/lint.out"

release: $(TARGETS) | $(RELEASE)
	@cp "$(TARGETS)" "$(RELEASE)/"
	@python -m zipfile -c "$(RELEASE).zip" "$(RELEASE)"

$(RELEASE):
	@mkdir -p $(RELEASE)

clean-release: | $(RELEASE)
	@rm -f $(RELEASE).zip
	@rm -rf $(RELEASE)

clean: clean-release
	@rm -rf $(BUILD_DIR)
