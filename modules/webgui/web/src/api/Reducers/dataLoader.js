import {
  actionTypes
} from '../Actions/actionTypes';

export const dataLoader = (state = {}, action) => {
  switch (action.type) {
    case actionTypes.setDataLoaderActivated:
      const {
        activated
      } = action.payload;

      return {
        ...state,
        activated
      }

    case actionTypes.setSelectedFilesPathName:
      const {
        selectedFilePaths
      } = action.payload;

      return {
        ...state,
        selectedFilePaths
      }

    case actionTypes.setSelectedFilesMetaData:
      const {
        selectedDataMetaData
      } = action.payload;

      return {
        ...state,
        selectedDataMetaData
      }

    case actionTypes.setVolumesConvertedCount:
      const {
        currentVolumesConvertedCount
      } = action.payload;

      return {
        ...state,
        currentVolumesConvertedCount
      }

    case actionTypes.setVolumesToConvertCount:
      const {
        currentVolumesToConvertCount
      } = action.payload;

      return {
        ...state,
        currentVolumesToConvertCount
      }

    default:
      return state;
  }
}
