/**
 * Utility method to iterate over elements in an Array that
 * internally uses the "forEach" property of the array.
 *
 * <p>
 * If the input Array is null/undefined then nothing is done.
 *
 * <p>
 * If the input object does not have a "forEach" method then
 * it is converted to a single element Array and iterated over.
 *
 *
 * @param  {Array|Object} a An Array or an Object
 * @param  {Function} fun The callback function for each property
 * @param  {Object} thisp The "this" object to use for the callback function
 * @return {void}
 */
module.exports = function(a, func, thisp) {
    if (a != null) {
        (a.forEach ? a : [a]).forEach(func, thisp);
    }
};