import { combineReducers } from 'redux'
import envelopes from './envelopes'
import histograms from './histogram'

//Add more reducers here
const openspaceApp = combineReducers({
  envelopes,
  histograms,
});

export default openspaceApp;
