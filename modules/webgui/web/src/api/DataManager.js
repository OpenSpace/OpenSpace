import Subscription from './Subscription';
import Connection from './Connection';

const TOPIC_TYPES = {
  get: 'get',
  interaction: 'interaction',
  set: 'set',
  subscribe: 'subscribe',
};

function UnknownTypeException(message, type = '') {
  this.message = message;
  this.type = type;
  this.allowedTypes = TOPIC_TYPES;
}

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
    const message = this.wrapMessage({
      type: TOPIC_TYPES.get,
      payload: {
        propertyUri: key,
      },
    });
    const callback = (...args) => {
      this.connection.clearTopic(message.topic);
      receivingCallback(...args);
    };
    this.send(message, callback);
  }

  setValue(key, value) {
    const message = this.wrapMessage({
      type: TOPIC_TYPES.get,
      payload: {
        setProperty: key,
        value,
      },
    });
    this.send(message);
  }

  createSubscription(key) {
    const message = this.wrapMessage({
      type: TOPIC_TYPES.subscribe,
      payload: {
        subscriptionProperty: key,
      },
    });
    const subscription = new Subscription(key, message.topic);
    const resubscribeOnReconnect = true;
    this.send(message, subscription.onMessage, resubscribeOnReconnect);
    return subscription;
  }

  /**
   * wrap the Connection.send method
   * @param message - the message to send
   * @param args - optional args to send Connection.send
   */
  send(message, ...args) {
    this.connection.send(message, ...args);
  }

  /**
   * Wrap a message to be sent
   *
   * @param payload - the payload to send
   * @param type - the topic type to use one of TOPIC_TYPES
   * @param topic - optional. the topicID to use.
   *
   * @returns {{payload: *, type: *, topic: *}}
   * @throws UnknownTypeException - unless provided `type` is in `TOPIC_TYPES`
   */
  wrapMessage({ payload, type, topic = this.nextTopicId }) {
    if (!TOPIC_TYPES[type]) {
      throw new UnknownTypeException('Unknown type provided.', type);
    }
    return { payload, type, topic };
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

// Export as a singleton
const instance = new DataManager();
Object.seal(instance);

export const TopicTypes = TOPIC_TYPES;
export default instance;
