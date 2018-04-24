import { actionTypes } from '../Actions/actionTypes';
import { fetchDataDone, fetchDataFailed } from "../Actions";
import fetch from 'node-fetch';
import {dataEndpointKey} from "../keys";

const fetchingInfo = (dispatch, id) => {
  fetch(`${dataEndpointKey}${id}.json`)
    .then(res => res.json())
    .then(data => {
      dispatch(fetchDataDone(data, id));
    })
    .catch(error => {
      console.log(error);
      dispatch(fetchDataFailed(id));
    });
};

export const fetchData = store => next => (action) => {
  const result = next(action);
  switch (action.type) {
    case actionTypes.fetchData:
      fetchingInfo(store.dispatch, action.payload.id);
      break;
    default:
      break;
  }
  return result;
};

