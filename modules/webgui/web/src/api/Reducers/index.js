import { combineReducers } from 'redux';
import { propertyOwner as propertyTree } from './propertyTree';
import { connection } from './connection';
import { fetchData} from './fetchData';

// Add more reducers here
const openspaceApp = combineReducers({
  propertyTree,
  connection,
  fetchData,
});

export default openspaceApp;
