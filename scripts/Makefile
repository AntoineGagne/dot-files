INSTALL_DIR = $(HOME)/.local/bin
SCRIPTS = bash/cmus-status.bash \
		  bash/control-volume.bash \
		  bash/create-named-pipe.bash \
		  bash/printscreen.bash \
		  python/cmus_parse.py
INSTALL_SCRIPTS := $(SCRIPTS:%=install-%)

.PHONY: all
all: $(INSTALL_SCRIPTS)

.PHONY: $(INSTALL_SCRIPTS)
$(INSTALL_SCRIPTS):
	# @chmod +x "$(@:install-%=%)"
	# @ln -sf "$(shell pwd)/$(@:install-%=%)" "$(INSTALL_DIR)/$(subst _,-,$(notdir $(basename $(@:install-%=%))))"

.PHONY: subdirs $(SCRIPTS)
$(SCRIPTS): $(INSTALL_SCRIPTS)
