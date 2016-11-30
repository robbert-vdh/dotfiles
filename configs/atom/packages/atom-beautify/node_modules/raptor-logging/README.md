raptor-logging
==============

Super simple logging system that works on the server and and in the browser.

# Example

___projects/logging-test/run.js:___

```javascript
require('raptor-logging').configure({
    loggers: {
        'ROOT': 'WARN',
        'logging-test': 'DEBUG'
    }
});

// ...

var logger = require('raptor-logging').logger(module);
logger.debug('This is a debug message');
```

Output:

```
DEBUG logging-test/run: This is a debug message
```

You can also pass multiple arguments, as well as non-String arguments to the logging methods.

For example:

```javascript
var logger = require('raptor-logging').logger(module);
logger.debug('This is a debug message', {foo: 'bar'});
```

Output:

```
DEBUG logging-test/run: This is a debug message { foo: 'bar' }
```

# Installation

```bash
npm install raptor-logging --save
```

# API

## logger(module)

Returns a new `Logger` instance whose name is based on the filename associated with the Node.js module object.

Example:

```javascript
var logger = require('raptor-logging').logger(module);
logger.debug('Hello World');
```

## logger(name)

Returns a new `Logger` instance with the given name

Example:

```javascript
var logger = require('raptor-logging').logger('foo');
logger.debug('Hello World');
```

Output:

```
DEBUG foo: Hello World
```

## configure(options)

Supported options:

- `loggers`: A mapping of logger prefixes to log levels (see below)
- `appenders`: An array of appender instances (see [ConsoleAppender](./lib/ConsoleAppender.js) for an example appender implementation)

Example:

```javascript
require('raptor-logging').configure({
    'ROOT': 'WARN',
    'foo': 'DEBUG',
    'foo/bar': 'WARN',
});
```

## Logger

Methods:

- isTraceEnabled() : boolean
- isDebugEnabled() : boolean
- isInfoEnabled() : boolean
- isWarnEnabled() : boolean
- isErrorEnabled() : boolean
- isFatalEnabled() : boolean
- dump(arg1, arg2, ...)
- trace(arg1, arg2, ...)
- debug(arg1, arg2, ...)
- info(arg1, arg2, ...)
- warn(arg1, arg2, ...)
- error(arg1, arg2, ...)
- fatal(arg1, arg2, ...)

# Contributors

* [Patrick Steele-Idem](https://github.com/patrick-steele-idem) (Twitter: [@psteeleidem](http://twitter.com/psteeleidem))

# Contribute

Pull Requests welcome. Please submit Github issues for any feature enhancements, bugs or documentation problems.

# License

Apache License v2.0

