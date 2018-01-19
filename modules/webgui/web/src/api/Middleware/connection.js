import Connection from '../Connection';
import DataManager from '../DataManager';

/**
 * start reconnection attempts, at intervals ever increasing
 */
const tryToReconnect = (store) => {
  let { connectionWait } = this.store.connection;

  console.log('Attempting to connect in', connectionWait, 'ms.'); // eslint-disable-line
  setTimeout(() => {
    DataManager.connection.reconnect();
    connectionWait *= 2;
    store.dispatch(changeConnectionWait(connectionWait));
  }, connectionWait);
}

const connectionStatusCallback = (store, event, origin) => {
    store.getState();
    switch (origin) {
      case 'onOpen':
        // everything is all right!
        store.dispatch(onOpen());
        break;
      case 'onClose':
        store.dispatch(onClose());
        this.tryToReconnect(store);
        break;
      case 'onError':
        break;
      default:
        // unknown
    }
  }

const initializeConnection = (store) => {
    connection.addStatusCallback(({event, origin}) => {
    connectionStatusCallback(store, event, origin)
  });
}

const connection = store => next => action => {
  let result = next(action)
  var state = store.getState();
  console.log(state)
  switch(action.type) {
  	case 'CONNECTION_START':
      initializeConnection(store)
  		break;
    default:
      break;
  }
  return result
}