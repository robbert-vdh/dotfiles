var slice = [].slice;

module.exports = function toArray(o) {
    if (o == null || Array.isArray(o)) {
        return o;
    }

    if (typeof o === 'string') {
        return o.split('');
    }

    if (o.length) {
        return slice.call(o, 0);
    }

    return [o];
};