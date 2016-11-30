var g = typeof window === 'undefined' ? global : window;
// Make this module a true singleton
module.exports = g.__RAPTOR_LOGGING || (g.__RAPTOR_LOGGING = require('./raptor-logging'));