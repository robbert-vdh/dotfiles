var g = typeof window === 'undefined' ? global : window;
// Make this module a true singleton
module.exports = g.__BROWSER_REFRESH_CLIENT || (g.__BROWSER_REFRESH_CLIENT = require('./browser-refresh-client'));