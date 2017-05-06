# PlatformIO layer

![PlatformIO logo](img/platformio.png)

## Table of Contents
  - [Description](#description)
  - [Install](#install)
  - [Key bindings](#key-bindings)

## Description
This layer adds support for PlatformIO. Install
the
[Irony-Mode layer](https://github.com/robbert-vdh/spacemacs-c-cpp-irony-layer)
for autocompletion support.

## Install
First of all, you should clone the layer to
`~/.emacs.d/private/layers/platformio`:

```shell
$ git clone https://github.com/robbert-vdh/spacemacs-platformio-layer ~/.emacs.d/private/layers/platformio
```

To use this configuration layer, add it to your `~/.spacemacs`. You will need to
add `platformio` to the existing `dotspacemacs-configuration-layers` list in
this file.

## Key bindings
| Key Binding | Description             |
| ----------- | ----------------------- |
| `SPC m p b` | Build                   |
| `SPC m p u` | Upload                  |
| `SPC m p p` | Upload using Programmer |
| `SPC m p s` | Upload SPIFFS           |
| `SPC m p c` | Clean                   |
| `SPC m p d` | Update                  |
| `SPC m p i` | Update Workspace        |
