import { onOpenConnection, changeConnectionWait, onCloseConnection } from '../Actions';
import { actionTypes } from '../Actions/actionTypes';

/**
 * start reconnection attempts, at intervals ever increasing
 */
const tryToReconnect = (store) => {
  const { connection } = store.getState();

  console.log('Attempting to connect in', connection.connectionWait, 'ms.'); // eslint-disable-line
  setTimeout(() => {
    connection.connection.reconnect();
    connection.connectionWait *= 2;
    store.dispatch(changeConnectionWait(connection.connectionWait));
  }, connection.connectionWait);
};

const connectionStatusCallback = (store, event, origin) => {
  store.getState();
  switch (origin) {
    case 'onOpen':
      // everything is all right!
      store.dispatch(onOpenConnection());
      break;
    case 'onClose':
      store.dispatch(onCloseConnection());
      tryToReconnect(store);
      break;
    case 'onError':
      break;
    default:
      // unknown
  }
};

const initializeConnection = (store) => {
  store.getState().connection.connection.addStatusCallback((connection, event, origin) => {
    connectionStatusCallback(store, event, origin);
  });
};

export const connection = store => next => (action) => {
  const result = next(action);
  switch (action.type) {
    case actionTypes.startConnection:
      initializeConnection(store);
      break;
    default:
      break;
  }
  return result;
};
