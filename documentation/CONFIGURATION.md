# Configuration

## `scripts/`

This directory contains all the useful scripts classified by programming languages (i.e. Python scripts in the `python/` folder).

## `bash/`

### Structure

The `bash/` folder has the following hierarchy:

```
bash/
├── .bash/
│   ├── aliases/
│   │   ├── date
│   │   ├── editing
│   │   ├── movement
│   │   ├── network
│   │   ├── programs
│   │   └── system
│   ├── .bash_aliases
│   ├── .bash_functions
│   └── functions/
│       ├── editing
│       ├── file
│       ├── misc
│       ├── movement
│       ├── processes
│       ├── system
│       └── youtube-dl
├── .bashrc
└── .profile
```

### `.bashrc`

Contains Bash general settings (i.e. prompt color, Git branch in prompt, etc.)

### `.bash/`

Contains all the aliases and functions.

#### `.bash_aliases`

Sources all the files containing aliases.

#### `aliases/`

Contains all the aliases. The aliases are in separate files according to the category they fit in. To define a new category, just create a file with the category's name.

#### `.bash_functions`

Sources all the files containing functions.

#### `functions/`

Contains all the functions. The functions are in separate files according to the category they fit in. To define a new category, just create a file with the category's name.

## `vim/`

Contains the settings for Vim and Neovim.

### Structure

The `.vim/` folder has the following hierarchy:

```
.vim/
├── after/
│   └── ftplugin/
├── bundle/
├── colors/
├── snippets/
├── spell/
└── startup/
    ├── autocommands.vim
    ├── commands.vim
    ├── functions.vim
    ├── mappings.vim
    ├── settings/
    │   ├── editor.vim
    │   ├── folds.vim
    │   ├── languages.vim
    │   ├── plugins/
    │   ├── plugins.vim
    │   ├── search.vim
    │   ├── spaces.vim
    │   └── ui.vim
    └── settings.vim
```

#### `after/`

This directory contains language specific configuration to be set after the plugins are loaded.

#### `bundle/`

This directory contains the plugins.

#### `colors/`

This directory contains the various colorschemes.

#### `snippets/`

This directory contains the custom snippets for `UltiSnips` or other snippets plugins.

#### `spell/`

This directory contains the spelling files with custom words.

#### `startup/`

This directory contains various editor configurations.

##### `autocommands.vim`

Contains the settings to be run on certain triggers.

##### `functions.vim`

Contains the various custom functions.

##### `mappings.vim`

Contains the custom mappings.

##### `settings`

The file `settings.vim` is only for including the specific settings.

###### `editor.vim`

Contains settings specific to editing.

###### `folds.vim`

Contains folds specific settings.

###### `languages.vim`

Contains languages specific settings.

###### `plugins/`

This directory contains the settings specific to plugins. When you want to specify settings for a specific plugin, you add a Vim file with the same name as the plugin and you include it in the `plugins.vim` file.

###### `search.vim`

Contains all the settings specific to searching.

###### `spaces.vim`

Contains all the settings specific to spaces.

###### `ui.vim`

Contains all the settings specific to UI such as colorschemes, drawing, etc.
