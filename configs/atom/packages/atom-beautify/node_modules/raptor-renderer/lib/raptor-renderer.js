'use strict';
var asyncWriter = require('async-writer');
var RenderResult = require('./RenderResult');
var extend = require('raptor-util/extend');

 function createRenderFunc(renderer) {
    return function render(input, out, callback) {
        // NOTE: we avoid using Function.apply for performance reasons
        switch (arguments.length) {
            case 0:
                // Arguments: input
                return exports.render(renderer);
            case 1:
                // Arguments: input
                return exports.render(renderer, input);
            case 2:
                // Arguments: input, out|callback
                return exports.render(renderer, input, out);
            case 3:
                // Arguments: input, out, callback
                return exports.render(renderer, input, out, callback);
            default:
                throw new Error('Illegal arguments');
        }
    };
}

exports.render = function (renderer, input, out) {
    var numArgs = arguments.length;
    // The renderer function is required so only set the callback if we have more
    // than one argument
    var callback;
    if (numArgs > 1) {
        callback = arguments[numArgs - 1];
    }

    var actualOut = out;
    var actualData = input || {};
    var shouldEnd = false;

    if (typeof callback === 'function') {
        // found a callback
        if (numArgs === 3) {
            actualOut = asyncWriter.create();
            shouldEnd = true;
        }
    } else {
        callback = null;
        if (!actualOut) {
            actualOut = asyncWriter.create();
            shouldEnd = true;
        }
    }

    var $global = actualData.$global;
    if ($global) {
        extend(actualOut.global, $global);
        delete actualData.$global;
    }

    if (typeof renderer !== 'function') {
        var renderFunc = renderer.renderer || renderer.render || renderer.process;

        if (typeof renderFunc !== 'function') {
            throw new Error('Invalid renderer');
        }

        renderFunc.call(renderer, actualData, actualOut);
    } else {
        renderer(actualData, actualOut);
    }

    if (callback) {
        actualOut
            .on('finish', function() {
                callback(null, new RenderResult(actualOut.getOutput(), actualOut));
            })
            .on('error', callback);
        if(shouldEnd) actualOut.end();
    } else {
        // NOTE: If no callback is provided then it is assumed that no asynchronous rendering occurred.
        //       Might want to add some checks in the future to ensure the actualOut is really done
        if(shouldEnd) actualOut.end();
        return new RenderResult(actualOut.getOutput(), actualOut);
    }
};

exports.renderable = function(target, renderer) {
    target.renderer = renderer;
    target.render = createRenderFunc(renderer);
};

exports.createRenderFunc = createRenderFunc;
