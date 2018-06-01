import { actionTypes } from './actionTypes';

export const setActivated = (isActivated) => ({
  type: actionTypes.setDataLoaderActivated,
  payload: {
    activated: isActivated
  }
});

export const setFilePaths = (filePaths) => ({
  type: actionTypes.setSelectedFilesPathName,
  payload: {
    filePaths: filePaths
  }
});
