import Connection from '../Connection';
import DataManager from '../DataManager';
import { actionTypes } from '../Actions/actionTypes'

export const connection = (state={}, action) => {  // state refers to connection
  switch(action.type) {
    case 'CONNECTION_START':
    	return {
		  connection: DataManager.connection,
		  url: Connection.DEFAULT_URL,
	      isConnected: false,
	      connectionLost: false,
	      connectionWait: 1000,
    	}
    case 'CONNECTION_ON_OPEN':
    	return {
		  ...state,
	      isConnected: true, 
	      connectionLost: false, 
	      connectionWait: 1000,
    	}
    case 'CONNECTION_ON_CLOSE':
    	return {
		  ...state,
		  isConnected: false, 
		  connectionLost: true
    	}
     case 'CONNECTION_CHANGE_WAIT':
    	return {
		  ...state,
		  connectionWait: action.payload.value,
    	}
    default:
    	return {
    		...state,
    	}
      break;
   }
}