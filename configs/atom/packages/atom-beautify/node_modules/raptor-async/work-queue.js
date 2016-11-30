function WorkQueue(options, runner) {
    if (arguments.length === 1) {
        runner = options;
        options = undefined;
    }

    var self = this;

    // an array of tasks
    self._tasks = [];

    // point before the first item
    self._pos = -1;

    // work queue starts paused
    self._paused = true;

    // work queue is initially empty so there is not a pending task
    self._pending = false;

    // runner is the function that will be called to run task
    self._runner = runner;

    self._onFinishTask = function(err) {
        self._pending = false;

        if (options) {
            if (err) {
                // error occured
                if (options.onTaskError) {
                    options.onTaskError.call(self._curTask);
                }
            } else {
                // job finished successfully
                if (options.onTaskComplete) {
                    options.onTaskComplete.call(self._curTask);
                }
            }
        }

        if (!self._paused) {
            // if we are not paused then go to the next
            self._next();
        }
    };
}

var proto = WorkQueue.prototype;

proto.push = function (task) {
    this._tasks.push(task);

    // start the job if we're not paused and we're not doing any work already
    if (!this._paused && (this._pos === -1)) {
        this._next();
    }
};

proto._next = function () {
    var self = this;

    // go to next job
    self._pos++;

    if (self._pos >= self._tasks.length) {
        // done
        self._pos = -1;

        // clear out the old work
        self._tasks = [];
    } else {
        var task = self._curTask = self._tasks[self._pos];

        // remove reference to task
        delete self._tasks[self._pos];

        // we will have a task that is waiting to be completed
        self._pending = true;

        // invoke the job and provide on completion callback;
        self._runner(task, self._onFinishTask);
    }
};

proto.resume = function() {
    this._paused = false;
    if (!this._pending) {
        this._next();
    }
};

proto.idle = function() {
    return !this._pending && ((this._pos + 1) >= this._tasks.length);
};

proto.kill = function() {
    this._pending = false;
    this._tasks = [];
    this._pos = -1;
};

module.exports = {
    create: function(options, runner) {
        return new WorkQueue(options, runner);
    }
};