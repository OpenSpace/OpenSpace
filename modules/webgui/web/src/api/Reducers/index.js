import { combineReducers } from 'redux';
import { propertyOwner as propertyTree } from './propertyTree';
import { connection } from './connection';
import { dataLoader } from './dataLoader';

// Add more reducers here
const openspaceApp = combineReducers({
  propertyTree,
  connection,
  dataLoader,
});

export default openspaceApp;
