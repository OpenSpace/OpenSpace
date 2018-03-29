// @flow
import Subscription from './Subscription';
import Connection from './Connection';

const TOPIC_TYPES = {
  get: 'get',
  interaction: 'interaction',
  luascript: 'luascript',
  set: 'set',
  subscribe: 'subscribe',
  time: 'time',
  trigger: 'trigger',
};

const SUBSCRIPTION_EVENTS = {
  start: 'start_subscription',
  stop: 'stop_subscription',
}

const ResubscribeOnReconnect = true;
Object.freeze(ResubscribeOnReconnect);

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
  topicId: number;
  subscriptions: Object;
  privateConnection: Connection;

  constructor() {
    this.topicId = 1;
    this.subscriptions = {};
    this.privateConnection = undefined;
  }

  /**
   * Subscribe to a property.
   * @param key - the uri of the property
   * @param callback (function) - callback to handle the incoming subscription
   */
  subscribe(key: string, callback: Function, type: string = TOPIC_TYPES.subscribe) {
    let subscription = this.subscriptions[key];
    if (!subscription) {
      subscription = this.createSubscription(key, type, SUBSCRIPTION_EVENTS.start);
      this.subscriptions[key] = subscription;
    }
    return subscription.addCallback(callback); 
  }

  /**
   * Unsubscribe from a property
   * @param key - the key to unsubscribe from
   * @param callback - the callback stored in the subscription
   */
  unsubscribe(key: string, callbackId: number) {
    const subscription = this.subscriptions[key];
    if (subscription) {
      subscription.removeCallback(callbackId);
      // TODO: should subscriptions be killed or should we let them be for future snappiness?
       if (!subscription.hasAnyCallbacks()) {
          this.deleteSubscription(key, subscription.topic);
          this.connection.clearTopic(subscription.topic);
          delete this.subscriptions[subscription.key];
       }
       return true;
    }
  }

  /**
   * Get a property from OpenSpace
   * @param key - the URI/key to get
   * @param receivingCallback - the callback that takes care of the response
   */
  getValue(key: string, receivingCallback: Function) {
    const message = this.wrapMessage({
      type: TOPIC_TYPES.get,
      payload: {
        property: key,
      },
    });
    const callback = (...args) => {
      this.connection.clearTopic(message.topic);
      receivingCallback(...args);
    };
    this.send(message, callback);
  }

  /**
   * Set the value of a property
   * @param key - the URI of the property
   * @param value - the value
   */
  setValue(key: string, value: mixed) {
    const message = this.wrapMessage({
      type: TOPIC_TYPES.set,
      payload: {
        property: key,
        value,
      },
    });
    this.send(message);
  }

  /**
   * trigger a trigger property
   * @param key - the uri
   */
  trigger(key: string) {
    const message = this.wrapMessage({
      type: TOPIC_TYPES.trigger,
      payload: {
        property: key,
      },
    });
    this.send(message);
  }

  /**
   * Run a lua script
   * @param script - the script to run
   */
  runScript(script: string) {
    const message = this.wrapMessage({
      type: TOPIC_TYPES.luascript,
      payload: {
        script,
      },
    });
    this.send(message);
  }

  createSubscription(key: string, type: string = TOPIC_TYPES.subscribe) {
    const message = this.wrapMessage({
      type,
      payload: {
        property: key,
        event: SUBSCRIPTION_EVENTS.start,
      },
    });
    const subscription = new Subscription(key, message.topic);
    this.send(message, subscription.onMessage, ResubscribeOnReconnect);
    return subscription;
  }

  deleteSubscription(key: string, topic: number, type: string = TOPIC_TYPES.subscribe) {
    const message = this.wrapMessage({
      type,
      payload: {
        property: key,
        event: SUBSCRIPTION_EVENTS.stop,
      },
      topic,
    });
    this.send(message);
  }

  /**
   * wrap the Connection.send method
   * @param message - the message to send
   * @param args - optional args to send Connection.send
   */
  send(message: Object, ...args: Array<mixed>) {
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

  get nextTopicId(): number {
    const { topicId } = this;
    this.topicId += 1;
    return topicId;
  }

  get connection(): Connection {
    this.privateConnection = this.privateConnection || new Connection();
    return this.privateConnection;
  }

  set connection(newConnection: Connection) {
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
