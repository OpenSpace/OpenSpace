/* globals WebSocket */

const DEFAULT_URL = 'ws://localhost:8001';
const TOPIC_KEY = 'topic';

class Connection {
  /**
   * Start a connection to the web socket server
   * @param url - the server url
   */
  constructor(url = DEFAULT_URL) {
    this.socket = new WebSocket(url);
    this.socket.onopen = this.onOpen.bind(this);
    this.socket.onclose = this.onClose.bind(this);
    this.socket.onmessage = this.onMessage.bind(this);
    this.commandQueue = [];
    this.topics = {};
  }

  onOpen(event) {
    console.log('Opened socket connection...', event);

    // flush command queue, save the messages that still fail to send
    this.commandQueue = this.commandQueue.filter(message => !this.sendOrStore(message));
  }

  onClose(event) {
    console.log('Closed socket connection...', event);
    // TODO: Tell datamanager connection was lost, take care of callbacks somehow

    console.log('Stored messages in command queue:', this.commandQueue);
  }

  /**
   * Callback for all messages on this connection
   * @param event
   */
  onMessage(event) {
    const message = JSON.parse(event.data);
    const topicId = message[TOPIC_KEY];
    const topicCallback = this.topics[topicId];

    if (topicId && topicCallback) {
      topicCallback(message);
    } else if (topicId) {
      // eslint-disable-next-line no-console
      console.error(`Could not find topic ${topicId}. Dropping message.`, event);
    } else {
      // consider this some sort of broadcast from the server
    }
  }

  /**
   * Send a message to the server
   * @param payload [object] - the payload to send to the server
   * @param callback [[function]] - This replaces any existing callback. Expects response.
   */
  send(payload, callback) {
    const message = JSON.stringify(payload);
    const { topic } = payload;

    // store callback
    if (callback && topic) {
      this.topics[topic] = callback;
    }

    // send it!
    this.sendOrStore(message);
  }

  /**
   * meant to be private method storing or sending an already prepared message
   * @param message
   * @return {boolean} true on send, false on store
   */
  sendOrStore(message) {
    if (this.isOpen()) {
      this.socket.send(message);
      return true;
    } else {
      this.commandQueue.push(message);
      return false;
    }
  }

  /**
   * Remove topic callback
   * @param topicId - the topicId
   * @returns {boolean} - true if callback was removed, false otherwise
   */
  clearTopic(topicId) {
    if (this.topics[topicId]) {
      delete this.topics[topicId];
      return true;
    }
    return false;
  }

  isConnecting() {
    return this.socket.readyState === this.socket.CONNECTING;
  }

  isOpen() {
    return this.socket.readyState === this.socket.OPEN;
  }

  isClosed() {
    return [this.socket.CLOSED, this.socket.CLOSING].includes(this.socket.readyState);
  }
}

export default Connection;
