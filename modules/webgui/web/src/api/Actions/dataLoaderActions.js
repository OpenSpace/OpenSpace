import { actionTypes } from './actionTypes';

export const toggleActivated = (activated) => ({
  type: actionTypes.toggleDataLoaderActivated,
  payload: {
    activated
  }
});
