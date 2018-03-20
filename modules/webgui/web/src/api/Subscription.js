class Subscription {
  constructor(key, topic) {
    this.key = key;
    this.topic = topic;
    this.callbacks = new Map();
    this.lastData = null;
    this.callbackId = -1;
    this.onMessage = this.onMessage.bind(this);
  }

  onMessage(...args) {
    this.lastData = args;
    for (const cb of this.callbacks.values()) {
      cb(...args);
    }
  }

  addCallback(callback) {
    // don't add the same callback twice
    if (this.hasCallback(callback)) return;

    this.callbackId = this.callbackId + 1;
    this.callbacks.set(this.callbackId, callback);
    // if we have stored some data -- send it to the callback
    if (this.lastData) {
      callback(...this.lastData);
    }
    return this.callbackId;
  }

  hasCallback(callback) {
    for (const cb of this.callbacks.values()) {
      if (cb === callback) {
        return true;
      }
    }
    return false;
  }

  removeCallback(callbackIndex) {
    this.callbacks.delete(callbackIndex);
  }

  hasAnyCallbacks() {
    return this.callbacks.size !== 0;
  }
}

export default Subscription;
