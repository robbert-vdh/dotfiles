raptor-pubsub
=============

Super lightweight module to support [EventEmitter](http://nodejs.org/api/events.html)-based Pub/Sub communication in the browser and on the server.

# Installation

```
npm install raptor-pubsub --save
```

# Usage

## Communicating on the global pub/sub channel

```javascript
var raptorPubsub = require('raptor-pubsub');

// Subscribe to an event
raptorPubsub.on('someEvent', function(arg) {
  // Do something...
});

// Publish an event
raptorPubsub.emit('someEvent', 'Hello World');
```

## Communicating on named pub/sub channels

```javascript
// Get a reference to named pub/sub channel
var channel = require('raptor-pubsub').channel('my-channel');

// Subscribe to an event
channel.on('someEvent', function(arg) {
  // Do something...
});

// Publish an event
channel.emit('someEvent', 'Hello World');

// Removing a channel
require('raptor-pubsub').removeChannel('my-channel');
```

The global pub/sub channel and named channels are simply [EventEmitter](http://nodejs.org/api/events.html#events_class_events_eventemitter) instances.

# Contributors

* [Patrick Steele-Idem](https://github.com/patrick-steele-idem) (Twitter: [@psteeleidem](http://twitter.com/psteeleidem))

# Contribute

Pull Requests welcome. Please submit Github issues for any feature enhancements, bugs or documentation problems.

# License

Apache License v2.0
