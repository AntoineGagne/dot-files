INSTALL_DIR = $(HOME)/.local/bin
SCRIPTS = bash/cmus-status.bash \
		  bash/control-volume.bash \
		  bash/create-named-pipe.bash \
		  bash/printscreen.bash \
		  python/cmus_parse.py \
		  bash/clean-local-tmp.sh \
		  bash/control-brightness.bash \
		  bash/unmount-device.sh \
		  python/display_screens.py \
		  bash/create-tmux-session.bash \
		  bash/create-tmux-sessions.bash \
		  bash/recompile-xmonad.sh \
		  bash/spawn-rtv.sh \
		  bash/merge-rss-urls.bash \
		  bash/corsair-keyboard.bash \
		  bash/webman.bash \
		  bash/cppman.bash \
		  bash/haskellman.bash \
		  bash/display-song-information.bash \
		  bash/notification-send.bash
INSTALL_SCRIPTS := $(SCRIPTS:%=install-%)
MANPAGES_DIR := $(HOME)/.local/share/man/man1
MANPAGES := $(patsubst %, $(MANPAGES_DIR)/%.1, control-brightness control-volume)

.PHONY: all
all: $(INSTALL_SCRIPTS) manpages

.PHONY: $(INSTALL_SCRIPTS)
$(INSTALL_SCRIPTS): $(INSTALL_DIR)
	@chmod +x "$(shell pwd)/$(@:install-%=%)"
	@ln -sf "$(shell pwd)/$(@:install-%=%)" "$(INSTALL_DIR)/$(subst _,-,$(notdir $(basename $(@:install-%=%))))"

.PHONY: $(INSTALL_DIR)
$(INSTALL_DIR):
	@mkdir -p "$(INSTALL_DIR)"

.PHONY: manpages
manpages: $(MANPAGES) | $(MANPAGES_DIR)

$(MANPAGES_DIR)/%.1: manpages/%.1 | $(MANPAGES_DIR)
	cp $< $@

.PHONY: $(MANPAGES_DIR)
$(MANPAGES_DIR):
	@mkdir -p $(MANPAGES_DIR)
