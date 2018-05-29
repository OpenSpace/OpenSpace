import { actionTypes } from '../Actions/actionTypes';

export const dataLoader = (state = {}, action) => { 
  switch(action.type) {
    case actionTypes.toggleDataLoaderActivated:
      const { activated } = action.payload;

      return {
        ...state,
        activated
      }

    default:
      return state;
  }
}
