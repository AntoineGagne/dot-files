SYSTEMD_DIR := systemd-units
SYSTEMD_CONFIG_DIR := ${HOME}/.config/systemd/user
UNITS := clean-local-tmp.service \
		 clean-local-tmp.timer \
		 mpdstats.service \
		 dunst.service
SYSTEMD_UNITS := $(addprefix $(SYSTEMD_CONFIG_DIR)/, $(UNITS))

VIRTUALENVS_DIR := $(HOME)/.virtualenvs
NEOVIM_2 := $(HOME)/.virtualenvs/neovim2
PYTHON_2 := $(shell command -v python2 2>/dev/null)
NEOVIM_3 := $(HOME)/.virtualenvs/neovim3
PYTHON_3 := $(shell command -v python3 2>/dev/null)

SOFTWARE_DIRS := bash \
	ctags \
	git \
	haskell \
	python \
	redshift \
	shell \
	vim \
	formatters \
	glances \
	cmus \
	latex \
	xbindkeys \
	urxvt \
	tmux \
	xfonts \
	Xresources \
	mutt \
	javascript \
	cpp \
	xmonad \
	xorg \
	locale \
	xmobar \
	zathura \
	feh \
	icons \
	weechat \
	compton \
	rtv \
	newsboat \
	alacritty \
	pandoc \
	tmuxp \
	beets \
	mpd \
	ncmpcpp \
	kitty \
	dunst \
	gdb
INSTALL_DIRS := $(SOFTWARE_DIRS:%=install-%)

.PHONY: all
all: $(SOFTWARE_DIRS) \
	 setup-virtual-environments \
	 install-scripts \
	 install-systemd-units

.PHONY: install-zsh
install-zsh:
	@mkdir -p zsh/.zsh
	@touch zsh/.zsh/histfile
	@stow zsh -t ~

.PHONY: install-scripts
install-scripts:
	@$(MAKE) -C scripts

.PHONY: subdirs $(SOFTWARE_DIRS)
$(SOFTWARE_DIRS): $(INSTALL_DIRS)

.PHONY: $(INSTALL_DIRS)
$(INSTALL_DIRS):
	@stow $(@:install-%=%) -t ~

.PHONY: setup-vim-plugins
setup-vim-plugins: install-vim
	@nvim +PlugInstall +qa
	@$(MAKE) -C $(HOME)/.vim/bundle/vimproc.vim

.PHONY: setup-virtual-environments
setup-virtual-environments: $(NEOVIM_2) $(NEOVIM_3)

$(NEOVIM_2): | install-virtual-environment-wrapper
ifdef PYTHON_2
	@virtualenv -p /usr/bin/python2 $(VIRTUALENVS_DIR)/neovim2 && pip install neovim
endif

$(NEOVIM_3): | install-virtual-environment-wrapper
ifdef PYTHON_3
	@virtualenv -p /usr/bin/python3 $(VIRTUALENVS_DIR)/neovim3 && pip install neovim
endif

.PHONY: install-virtual-environment-wrapper
install-virtual-environment-wrapper:  $(VIRTUALENVS_DIR)
	@pip install --user virtualenv virtualenvwrapper

$(VIRTUALENVS_DIR):
	@mkdir -p $(VIRTUALENVS_DIR)

.PHONY: install-language-servers
install-language-servers: install-python-language-server

install-python-language-server:
	@pip install --user python-language-server pyls-mypy

.PHONY: install-systemd-units
install-systemd-units: $(SYSTEMD_UNITS)

$(SYSTEMD_CONFIG_DIR)/%.service: $(SYSTEMD_DIR)/%.service | $(SYSTEMD_CONFIG_DIR)
	@cp $< $(SYSTEMD_CONFIG_DIR)

$(SYSTEMD_CONFIG_DIR)/%.timer: $(SYSTEMD_DIR)/%.timer | $(SYSTEMD_CONFIG_DIR)
	@cp $< $(SYSTEMD_CONFIG_DIR)

$(SYSTEMD_CONFIG_DIR):
	@mkdir -p $(SYSTEMD_CONFIG_DIR)

.PHONY: install-themes
install-themes:
	@stow gtk -t ~

.PHONY: fonts
fonts:
	@stow fonts -t ~

.PHONY: install-fonts
install-fonts: fonts
	@fc-cache -fv fonts/.fonts
