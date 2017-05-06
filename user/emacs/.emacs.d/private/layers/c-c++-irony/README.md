# C/C++ Irony layer

## Table of Contents
  - [Description](#description)
  - [Install](#install)
  - [Key bindings](#key-bindings)

## Description
This layer replaces `company-clang`
with [Irony-Mode](https://github.com/Sarcasm/irony-mode), providing much faster
auto completion using libclang and support for `.clang_complete` files.

## Install
First of all, you should clone the layer to
`~/.emacs.d/private/layers/c-c++-irony`:

```shell
$ git clone https://github.com/robbert-vdh/spacemacs-c-c++-irony-layer ~/.emacs.d/private/layers/c-c++-irony
```

To use this configuration layer, add it to your `~/.spacemacs`. You will need to
add `c-c++-irony` to the existing `dotspacemacs-configuration-layers` list in
this file.

Irony-Mode uses the C++98 standard by default. You can override this by
appending `"-std=c++<standard>"` to `irony-additional-clang-options` in your
user initialization or directory locals, e.g.:

```elisp
(setq irony-additional-clang-options '("-std=c++14"))
```

## Key bindings
None.
