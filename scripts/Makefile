INSTALL_DIR = $(HOME)/.local/bin
SCRIPTS = bash/cmus-status.bash \
		  bash/control-volume.bash \
		  bash/create-named-pipe.bash \
		  bash/printscreen.sh \
		  python/cmus_parse.py \
		  bash/clean-local-tmp.sh \
		  bash/fix-systemd-user-units.sh
INSTALL_SCRIPTS := $(SCRIPTS:%=install-%)

.PHONY: all
all: $(INSTALL_SCRIPTS)

.PHONY: $(INSTALL_SCRIPTS)
$(INSTALL_SCRIPTS): $(INSTALL_DIR)
	@chmod +x "$(shell pwd)/$(@:install-%=%)"
	@ln -sf "$(shell pwd)/$(@:install-%=%)" "$(INSTALL_DIR)/$(subst _,-,$(notdir $(basename $(@:install-%=%))))"

.PHONY: subdirs $(SCRIPTS)
$(SCRIPTS): $(INSTALL_SCRIPTS)

.PHONY: $(INSTALL_DIR)
$(INSTALL_DIR):
	@mkdir -p "$(INSTALL_DIR)"
