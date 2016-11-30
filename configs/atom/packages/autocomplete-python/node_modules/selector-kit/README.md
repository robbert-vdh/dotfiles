# Selector Kit [![Build Status](https://travis-ci.org/atom/selector-kit.svg?branch=master)](https://travis-ci.org/atom/selector-kit)

## Usage

```coffee
{Selector} = require 'selector-kit'

# Creates one Selector
[someNodeSelector] = Selector.create('.some-node')

# Creates two selectors, due to the `,`
[someNodeSelector, anotherNodeSelector] = Selector.create('.some-node, .another-node')

someNodeSelector.matches('.parent .some-node') # => true
someNodeSelector.matches('.parent .whatever') # => false

anotherNodeSelector.matches('.parent .another-node') # => true
anotherNodeSelector.matches('.parent .whatever') # => false

# Other supported methods
someNodeSelector.toString() # => '.some-node'
someNodeSelector.getSpecificity() # => 20
someNodeSelector.compare(anotherNodeSelector) # => 0
someNodeSelector.isEqual(anotherNodeSelector) # false
```

:boom:
