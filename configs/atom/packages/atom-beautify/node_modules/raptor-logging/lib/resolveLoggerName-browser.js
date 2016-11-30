module.exports = function(module) {
    if (!module) {
        return '';
    }

    if (typeof module === 'string') {
        return module;
    }

    return module.id || module.toString();
};