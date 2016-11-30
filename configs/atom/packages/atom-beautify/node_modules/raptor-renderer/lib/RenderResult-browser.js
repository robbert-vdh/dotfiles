'use strict';
var dom = require('raptor-dom');
var raptorPubsub = require('raptor-pubsub');

function checkAddedToDOM(renderResult, method) {
    if (!renderResult._added) {
        throw new Error('Cannot call ' + method + '() until after HTML fragment is added to DOM.');
    }
}

function RenderResult(html, out) {
    this.html = html;
    this.out = out;
    this._node = undefined;

    var widgetsContext = out.global.widgets;
    this._widgetDefs = widgetsContext ? widgetsContext.widgets : null;
}

RenderResult.prototype = {
    getWidget: function () {
        checkAddedToDOM(this, 'getWidget');

        var rerenderWidget = this.out.__rerenderWidget;
        if (rerenderWidget) {
            return rerenderWidget;
        }

        var widgetDefs = this._widgetDefs;
        if (!widgetDefs) {
            throw new Error('No widget rendered');
        }
        return widgetDefs.length ? widgetDefs[0].widget : undefined;
    },
    getWidgets: function (selector) {
        checkAddedToDOM(this, 'getWidgets');

        var widgetDefs = this._widgetDefs;

        if (!widgetDefs) {
            throw new Error('No widget rendered');
        }

        var widgets;
        var i;
        if (selector) {
            // use the selector to find the widgets that the caller wants
            widgets = [];
            for (i = 0; i < widgetDefs.length; i++) {
                var widget = widgetDefs[i].widget;
                if (selector(widget)) {
                    widgets.push(widget);
                }
            }
        } else {
            // return all widgets
            widgets = new Array(widgetDefs.length);
            for (i = 0; i < widgetDefs.length; i++) {
                widgets[i] = widgetDefs[i].widget;
            }
        }
        return widgets;
    },
    afterInsert: function (document) {
        var node = this.getNode(document);

        this._added = true;
        raptorPubsub.emit('raptor-renderer/renderedToDOM', {
            node: node,
            context: this.out,
            out: this.out,
            document: node.ownerDocument
        });    // NOTE: This will trigger widgets to be initialized if there were any

        return this;
    },
    appendTo: function (referenceEl) {
        dom.appendTo(this.getNode(referenceEl.ownerDocument), referenceEl);
        return this.afterInsert();
    },
    replace: function (referenceEl) {
        dom.replace(this.getNode(referenceEl.ownerDocument), referenceEl);
        return this.afterInsert();
    },
    replaceChildrenOf: function (referenceEl) {
        dom.replaceChildrenOf(this.getNode(referenceEl.ownerDocument), referenceEl);
        return this.afterInsert();
    },
    insertBefore: function (referenceEl) {
        dom.insertBefore(this.getNode(referenceEl.ownerDocument), referenceEl);
        return this.afterInsert();
    },
    insertAfter: function (referenceEl) {
        dom.insertAfter(this.getNode(referenceEl.ownerDocument), referenceEl);
        return this.afterInsert();
    },
    prependTo: function (referenceEl) {
        dom.prependTo(this.getNode(referenceEl.ownerDocument), referenceEl);
        return this.afterInsert();
    },
    getNode: function (document) {
        var node = this._node;
        var curEl;
        var newBodyEl;
        document = document || window.document;
        if (node === undefined) {
            if (this.html) {
                newBodyEl = document.createElement('body');
                newBodyEl.innerHTML = this.html;
                if (newBodyEl.childNodes.length == 1) {
                    // If the rendered component resulted in a single node then just use that node
                    node = newBodyEl.childNodes[0];
                } else {
                    // Otherwise, wrap the nodes in a document fragment node
                    node = document.createDocumentFragment();
                    while ((curEl = newBodyEl.firstChild)) {
                        node.appendChild(curEl);
                    }
                }
            } else {
                // empty HTML so use empty document fragment (so that we're returning a valid DOM node)
                node = document.createDocumentFragment();
            }
            this._node = node;
        }
        return node;
    },
    toString: function() {
        return this.html;
    }
};
module.exports = RenderResult;