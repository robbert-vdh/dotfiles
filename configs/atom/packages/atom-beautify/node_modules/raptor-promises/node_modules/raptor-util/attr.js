var escapeXmlAttr = require('./escapeXml').attr;

module.exports = function(name, value, escapeXml) {
    if (value === true) {
        value = '';
    } else if (value == null || value === '' || value === false) {
        return '';
    } else {
        value = '="' + (escapeXml === false ? value : escapeXmlAttr(value)) + '"';
    }
    return ' ' + name + value;
};