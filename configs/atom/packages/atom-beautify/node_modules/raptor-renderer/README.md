raptor-renderer
===============

This module provides support for rendering UI components and templates and provides helper methods for injecting the resulting HTML into the DOM _and_ binding behavior.

In the browser, this module allows behavior to be attached by publishing a `raptor-renderer/renderedToDOM` message via the [raptor-pubsub](https://github.com/raptorjs/raptor-pubsub) module when the newly rendered DOM nodes have been inserted into the DOM. The [marko-widgets](https://github.com/raptorjs/marko-widgets) module listens for this event to initialize all widgets that were rendered. Internally, the list of rendered widgets is stored in the [async-writer](https://github.com/raptorjs/async-writer) instance (specifically `out.global.widgets`) that is passed to all renderers during the rendering process.

# Installation

```bash
npm install raptor-renderer --save
```

# Usage

```javascript
var raptorRenderer = require('raptor-renderer');

var renderer = function(input, out) {
    out.write('Hello ' + input.name + '!');
};

var targetEl = document.getElementById('myRenderTarget');

raptorRenderer.render(
    renderer,       // function renderer(input, out)
    {               // View model
        name: 'Frank'
    },
    function(err, renderResult) {
        if (err) {
            // Handle the error
        }

        // Append the HTML as a child of the provided target element
        renderResult.appendTo(targetEl);

        // Behavior (if any) has been attached automagically. Yay!

        // All available methods
        // - appendTo(el)
        // - replace(el)
        // - replaceChildrenOf(el)
        // - insertBefore(el)
        // - insertAfter(el)
        // - prependTo(el)
    });
```

If you know for sure that the rendering will complete synchronously then you can instead use the synchronous API by not providing a callback function:

```javascript
var raptorRenderer = require('raptor-renderer');

var renderer = function(input, out) {
    out.write('Hello ' + input.name + '!');
};

var targetEl = document.getElementById('myRenderTarget');

var renderResult = raptorRenderer.render(
    renderer,       // function renderer(input, out)
    {               // View model
        name: 'Frank'
    });

renderResult.appendTo(targetEl);
```

# API

## render(renderer, data, callback)

Invokes a renderer with the provided data and invokes the provided callback when the rendering asynchronously completes. The signature for the callback function is `function(err, renderResult)` where `renderResult` will be an instance of `RenderResult` (see below).


## render(renderer, data) : RenderResult

Synchronous version of the `render` method that immediately returns a `RenderResult` object.

## RenderResult

### Methods

#### appendTo(el)

Inserts the newly rendered DOM nodes as ending children of the target HTML element.

#### getWidget()

Returns the top-level widget (if any) associated with the rendered HTML. This method can only be called after the HTML has been inserted into the DOM (e.g. using `appendTo(document.body)`);

Example:

```javascript
var buttonWidget = require('raptor-renderer').render(
    buttonRenderer,
    {
        label: label
    })
    .appendTo(document.body)
    .getWidget();

buttonWidget.disable();
```

#### getWidgets()

Returns an array of all of the rendered widgets;

#### insertAfter(el)

Inserts the newly rendered DOM nodes as siblings immediately _after_ the target HTML element.

#### insertBefore(el)

Inserts the newly rendered DOM nodes as siblings immediately _before_ the target HTML element.

#### prependTo(el)

Inserts the newly rendered DOM nodes as beginning children of the target HTML element.

#### replace(el)

Removes the target element and replaces is with the newly rendered DOM nodes.

#### replaceChildrenOf(el)

Removes the child nodes of the target element and replaces them with the newly rendered DOM nodes.

#### toString()

Returns the output HTML string.
