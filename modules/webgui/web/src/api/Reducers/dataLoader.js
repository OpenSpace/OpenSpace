import { actionTypes } from '../Actions/actionTypes';

export const dataLoader = (state = {}, action) => { 
  switch(action.type) {
    case actionTypes.setDataLoaderActivated:
      const { activated } = action.payload;

      return {
        ...state,
        activated
      }

    case actionTypes.setSelectedFilesPathName:
      const { filePaths } = action.payload;
      
      return {
        ...state,
        filePaths
      }

    default:
      return state;
  }
}
