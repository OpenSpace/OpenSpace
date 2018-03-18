import Connection from '../Connection';
import DataManager from '../DataManager';
import { actionTypes } from '../Actions/actionTypes';

export const connection = (state = {}, action) => { // state refers to connection
  switch (action.type) {
    case actionTypes.startConnection:
      return {
        connection: DataManager.connection,
        url: Connection.DEFAULT_URL,
        isConnected: false,
        connectionLost: false,
        connectionWait: 1000,
      };
    case actionTypes.onOpenConnection:
      return {
        ...state,
        isConnected: true,
        connectionLost: false,
        connectionWait: 1000,
      };
    case actionTypes.onCloseConnection:
      return {
        ...state,
        isConnected: false,
        connectionLost: true,
      };
    case actionTypes.changeConnectionWait:
      return {
        ...state,
        connectionWait: action.payload.value,
      };
    default:
      return {
        ...state,
      };
  }
};
