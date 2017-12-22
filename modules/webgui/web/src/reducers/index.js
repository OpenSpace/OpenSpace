import { combineReducers } from 'redux'
import transferfunctions from './transferfunctions'
//Add more reducers here
const openspaceApp = combineReducers({
  transferfunctions,
});

export default openspaceApp;
