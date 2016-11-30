raptor-polyfil
===========
Polyfills for various EcmaScript 5 and EcmaScript 6 methods distributed as CommonJS modules that can be require'd individually or as a whole. For use with your favorite CommonJS module bundler (such as [RaptorJS Optimizer](https://github.com/raptorjs/optimizer)).

# Usage

## EcmaScript 5

To apply all polyfills (see below):

```javascript
require('raptor-polyfill')
```

You can also apply select groups of methods:

```javascript
require('raptor-polyfill/array');
require('raptor-polyfill/date');
require('raptor-polyfill/function');
require('raptor-polyfill/object');
require('raptor-polyfill/string');
```

Finally, you can apply individual methods:

```javascript

// Array methods
require('raptor-polyfill/array/every');
require('raptor-polyfill/array/filter');
require('raptor-polyfill/array/forEach');
require('raptor-polyfill/array/indexOf');
require('raptor-polyfill/array/isArray');
require('raptor-polyfill/array/lastIndexOf');
require('raptor-polyfill/array/map');
require('raptor-polyfill/array/reduce');
require('raptor-polyfill/array/reduceRight');
require('raptor-polyfill/array/some');

// Date methods
require('raptor-polyfill/date/now');

// Function methods
require('raptor-polyfill/function/bind');

// Object methods
require('raptor-polyfill/object/keys');

// String methods
require('raptor-polyfill/string/trim');
require('raptor-polyfill/string/startsWith');
require('raptor-polyfill/string/endsWith');
```
# Contributing

If you really want to add missing EcmaScript 5 or EcmaScript 6 methods please send a Pull Request.

# License

MIT

# Credits

Based on code from the following projects:

* [es5-shim](https://github.com/es-shims/es5-shim)
* [json.org](http://www.json.org/)
