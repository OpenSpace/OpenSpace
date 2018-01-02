import { combineReducers } from 'redux'
import { sceneGraph, settings } from './scenegraph'
//Add more reducers here
const openspaceApp = combineReducers({
  sceneGraph,
});

export default openspaceApp;
