import { combineReducers } from 'redux';
import { propertyOwner as propertyTree } from './propertyTree';
import { connection } from './connection';
import { fetchData} from './fetchData';
import { storyTree } from './storyTree';

// Add more reducers here
const openspaceApp = combineReducers({
  propertyTree,
  connection,
  fetchData,
  storyTree,
});

export default openspaceApp;
