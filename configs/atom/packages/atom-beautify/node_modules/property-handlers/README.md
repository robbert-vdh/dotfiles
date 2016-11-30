property-handlers
=================

# Installation
```
npm install property-handlers --save
```

# Usage
```javascript
var propertyHandlers = require('property-handlers');
propertyHandlers(object, handlers, path);
```

## Arguments
* `object`: An object with properties
* `handlers`: An object with properties that correspond to allowed properties. The value of each property should be a handler `function`
* `path`: Used for error reporting

## Example:
```javascript
var object = {
    foo: 'foo',
    bar: 'bar'
};

propertyHandlers(
    object,
    {
        foo: function(value) {
            // value === 'foo'
        },

        bar: function(value) {
            // bar === 'bar'
        }
    },
    'some path');
```