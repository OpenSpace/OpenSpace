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
