var attr = require('./attr');

module.exports = function(_attrs) {
    var out = '';
    for (var attrName in _attrs) {
        if (_attrs.hasOwnProperty(attrName)) {
            out += attr(attrName, _attrs[attrName]);
        }
    }
    return out;
};