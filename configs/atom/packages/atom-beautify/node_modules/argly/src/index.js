var Parser = require('./Parser');

exports.createParser = function(options) {
    var parser = new Parser();
    if (options) {
        parser.options(options);
    }
    return parser;
};