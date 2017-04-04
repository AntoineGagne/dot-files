# Installation

To install, you can use GNU `stow`. For example, to install the `vim` dotfiles, you can simply do the following:

```shell
stow vim -t ~
```

Alternatively, to install everything, you can simply run the command `make`.

## Fonts

To install the fonts, you can simply run the following command on the font directory:

```sh
fc-cache -fv .fonts
```
