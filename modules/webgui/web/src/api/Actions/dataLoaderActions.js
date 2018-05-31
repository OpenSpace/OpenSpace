import { actionTypes } from './actionTypes';

export const setActivated = (isActivated) => ({
  type: actionTypes.setDataLoaderActivated,
  payload: {
    activated: isActivated
  }
});
