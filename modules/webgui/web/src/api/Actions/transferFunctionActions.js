import { actionTypes } from './actionTypes';

const IdContainer = [];

export const addEnvelope = (points, URI) => {
  if (!(URI in IdContainer)) {
    IdContainer[URI] = 0;
  }
  return {
    type: actionTypes.changePropertyTreeNode,
    payload: {
      type: actionTypes.addEnvelope,
      id: IdContainer[URI]++,
      points,
      URI,
    },
  };
};

export const addPoint = (color, URI) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.addPoint,
    color,
    URI,
  },
});

export const deleteEnvelope = URI => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.deleteEnvelope,
    URI,
  },
});

export const clearEnvelopes = URI => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.clearEnvelopes,
    URI,
  },
});

export const movePoint = (deltaPosition, id, envelopeId, URI) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.movePoint,
    envelopeId,
    id,
    deltaPosition,
    URI,
  },
});

export const changeColor = (color, URI) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.changeColor,
    color,
    URI,
  },
});

export const toggleActiveEnvelope = (envelopeId, URI) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.toggleActiveEnvelope,
    envelopeId,
    URI,
  },
});

export const toggleActivePoint = (envelopeId, pointId, URI) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.toggleActivePoint,
    envelopeId,
    pointId,
    URI,
  },
});

export const setClickablePoint = (isClickable, envelopeId, pointId, URI) => ({
  type: actionTypes.changePropertyTreeNode,
  payload: {
    type: actionTypes.setClickablePoint,
    isClickable,
    envelopeId,
    pointId,
    URI,
  },
});
