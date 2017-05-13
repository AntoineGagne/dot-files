FONTS_DIR := fonts
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
	feh
INSTALL_DIRS := $(SOFTWARE_DIRS:%=install-%)

YOUCOMPLETEME_DIR := ${HOME}/.vim/bundle/YouCompleteMe
YOUCOMPLETEME_FLAGS := --tern-completer \
					   --racer-completer \
					   --clang-completer

.PHONY: all
all: $(SOFTWARE_DIRS) \
	 setup_youcompleteme \
	 setup_virtual_environments \
	 install-scripts

.PHONY: install-scripts
install-scripts:
	@$(MAKE) -C scripts

.PHONY: subdirs $(SOFTWARE_DIRS)
$(SOFTWARE_DIRS): $(INSTALL_DIRS)

.PHONY: $(INSTALL_DIRS)
$(INSTALL_DIRS):
	@stow $(@:install-%=%) -t ~

.PHONY: setup_vim_plugins
setup_vim_plugins: install-vim
	@mkdir -p ~/.tmp
	@nvim +PluginInstall +qa
	@$(MAKE) -C ~/.vim/bundle/vimproc.vim

.PHONY: setup_youcompleteme
setup_youcompleteme: setup_vim_plugins
	@$(shell exec $(YOUCOMPLETEME_DIR)/install.py $(YOUCOMPLETEME_FLAGS))

.PHONY: install_virtual_environment
install_virtual_environment:
	@pip3 install virtualenv virtualenvwrapper

.PHONY: setup_virtual_environments
setup_virtual_environments: install_virtual_environment
	@mkdir -p ~/.virtualenvs
	@mkvirtualenv neovim2 -p /usr/bin/python2
	@pip install neovim
	@mkvirtualenv neovim3 -p /usr/bin/python3
	@pip install neovim
	@deactivate

.PHONY: fonts
fonts:
	@stow fonts -t ~

.PHONY: install-fonts
install-fonts: fonts
	@fc-cache -fv fonts/.fonts
