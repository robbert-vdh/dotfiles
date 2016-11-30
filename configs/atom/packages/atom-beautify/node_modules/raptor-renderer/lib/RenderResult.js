'use strict';


function RenderResult(html, out) {
    this.html = html;
    this.out = this.context /* legacy */ = out;
}

RenderResult.prototype = {
    toString: function() {
        return this.html;
    }
};
module.exports = RenderResult;