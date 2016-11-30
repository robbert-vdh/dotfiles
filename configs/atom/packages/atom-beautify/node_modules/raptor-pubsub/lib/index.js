var g = typeof window === 'undefined' ? global : window;
// Make this module a true singleton
module.exports = g.__RAPTOR_PUBSUB || (g.__RAPTOR_PUBSUB = require('./raptor-pubsub'));