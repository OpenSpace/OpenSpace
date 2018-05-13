import { combineReducers } from 'redux';
import { propertyOwner as propertyTree } from './propertyTree';
import { connection } from './connection';

// Add more reducers here
const openspaceApp = combineReducers({
  propertyTree,
  connection,
});

export default openspaceApp;
