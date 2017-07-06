class Subscription {
  constructor(key, topic) {
    this.key = key;
    this.topic = topic;
    this.callbacks = [];
    this.lastData = null;
  }

  onMessage(...args) {
    this.lastData = args;
    this.callbacks.forEach(cb => cb(args));
  }

  addCallback(callback) {
    // don't add the same callback twice
    if (this.hasCallback(callback)) return;

    this.callbacks.push(callback);
    // if we have stored some data -- send it to the callback
    if (this.lastData) {
      callback(this.lastData);
    }
  }

  hasCallback(callback) {
    return this.callbacks.includes(callback);
  }

  removeCallback(callback) {
    const callbackIndex = this.callbacks.indexOf(callback);
    if (callbackIndex >= 0) {
      this.callbacks.splice(callbackIndex, 1);
    }
  }

  hasAnyCallbacks() {
    return this.callbacks.length > 0;
  }
}

export default Subscription;
