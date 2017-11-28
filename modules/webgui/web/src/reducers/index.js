import { combineReducers } from 'redux'
import envelopes from './envelopes'

//Add more reducers here
const openspaceApp = combineReducers({
  envelopes,
});

export default openspaceApp;
