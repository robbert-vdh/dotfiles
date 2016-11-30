browser-refresh-client
======================

Small module that allows the current child process launched by [browser-refresh](https://github.com/patrick-steele-idem/browser-refresh) to control the parent `browser-refresh` process. If the process was not launched by the `browser-refresh` launcher then operations will be a no-op.

# Installation

```bash
npm install browser-refresh-client --save
```

# Usage

## Check if the current process was launched using `browser-refresh`

```javascript
require('browser-refresh-client').isBrowserRefreshEnabled();
```


## Enable special reloading of files

```javascript
require('browser-refresh-client')
    .enableSpecialReload('*.foo *.bar')
    .onFileModified(function(path) {
        // Handle the modification of either a *.foo file or
        // a *.bar file...
    });
```
