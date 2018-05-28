import { actionTypes } from './actionTypes';

export const updatePropertyValue = (description, value) => ({
  type: actionTypes.updatePropertyTreeNode,
  payload: {
    URI: description.Identifier,
    description,
    value,
  },
});

export const changePropertyValue = (description, value) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    URI: description.Identifier,
    description,
    value,
  },
});

export const initializePropertyTree = node => ({
  type: actionTypes.insertNode,
  payload: {
    node,
  },
});


export const startListening = URI => ({
  type: actionTypes.startListeningToNode,
  payload: {
    URI,
  },
});

export const stopListening = URI => ({
  type: actionTypes.stopListeningToNode,
  payload: {
    URI,
  },
});

export const startConnection = () => ({
  type: actionTypes.startConnection,
  payload: {

  },
});

export const onOpenConnection = () => ({
  type: actionTypes.onOpenConnection,
  payload: {

  },
});

export const onCloseConnection = () => ({
  type: actionTypes.onCloseConnection,
  payload: {

  },
});

export const changeConnectionWait = value => ({
  type: actionTypes.changeConnectionWait,
  payload: {
    value,
  },
});

export const fetchData = id => ({
  type: actionTypes.fetchData,
  payload: {
    id,
  },
});

export const fetchDataDone = (data, id) => ({
  type: actionTypes.fetchDataDone,
  payload: {
    data,
    id,
  },
});

export const fetchDataFailed = id => ({
  type: actionTypes.fetchDataFailed,
  payload: {
    id,
  },
});

export const addStoryTree = story => ({
  type: actionTypes.addStoryTree,
  payload: {
    story,
  },
});

export const resetStoryTree = reset => ({
  type: actionTypes.resetStoryTree,
  payload: {
    reset,
  },
});

export const addStoryInfo = info => ({
  type: actionTypes.addStoryInfo,
  payload: {
    info,
  },
});

export const resetStoryInfo = () => ({
  type: actionTypes.resetStoryInfo,
  payload: {
  },
});
