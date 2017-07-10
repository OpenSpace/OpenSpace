import Subscription from './Subscription';
import Connection from './Connection';

const TOPIC_TYPES = {
  subscribe: 'subscribe',
  get: 'get',
  set: 'set',
};

/**
 * Send and receive data from the socket server. This allows you to
 * set, get and subscription to properties. Singleton.
 */
class DataManager {
  constructor() {
    this.topicId = 1;
    this.subscriptions = {};
    this.privateConnection = undefined;
  }

  subscribe(key, callback) {
    let subscription = this.subscriptions[key];
    if (!subscription) {
      subscription = this.createSubscription(key);
      this.subscriptions[key] = subscription;
    }
    subscription.addCallback(callback);
  }

  createSubscription(key) {
    const topic = this.nextTopicId;
    const payload = {
      topic,
      type: TOPIC_TYPES.subscribe,
      payload: {
        subscriptionProperty: key,
      },
    };
    const subscription = new Subscription(key, topic);
    this.connection.send(payload, subscription.onMessage);
    return subscription;
  }

  unsubscribe(key, callback) {
    const subscription = this.subscriptions[key];
    if (subscription) {
      subscription.removeCallback(callback);

      // TODO: should subscriptions be killed or should we let them be for future snappiness?
      // if (!subscription.hasAnyCallbacks()) {
      //   this.connection.clearTopic(subscription.topic);
      //   delete this.subscriptions[subscription.key];
      // }
    }
  }

  getValue(key, receivingCallback) {
    const topic = this.nextTopicId;
    const payload = {
      topic,
      type: TOPIC_TYPES.get,
      payload: {
        getProperty: key,
      },
    };
    const callback = (...args) => {
      this.connection.clearTopic(topic);
      receivingCallback(args);
    };
    this.connection.send(payload, callback);
  }

  setValue(key, value) {
    const payload = {
      topic: this.nextTopicId,
      type: TOPIC_TYPES.get,
      payload: {
        setProperty: key,
        value,
      },
    };
    this.connection.send(payload);
  }

  get nextTopicId() {
    const { topicId } = this;
    this.topicId += 1;
    return topicId;
  }

  get connection() {
    this.privateConnection = this.privateConnection || new Connection();
    return this.privateConnection;
  }

  set connection(newConnection) {
    if (this.privateConnection) {
      this.connection.close();
    }

    this.privateConnection = newConnection;
  }
}

const instance = new DataManager();
Object.seal(instance);

export default instance;
