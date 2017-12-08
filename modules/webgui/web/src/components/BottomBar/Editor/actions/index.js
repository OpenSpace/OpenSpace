let nextEnvelopeId = 0;
let nextHistogramId = 0;
export const addEnvelope = (points) => {
  return {
    type: 'ADD_ENVELOPE',
    id: nextEnvelopeId++,
    points,
  };
};

export const addPoint = (color) => {
  return {
    type: 'ADD_POINT',
    color,
    id: nextEnvelopeId++,
  };
};

export const deleteEnvelope = () => {
  return {
    type: 'DELETE_ENVELOPE',
  };
};

export const clearEnvelopes = () => {
  return {
    type: 'CLEAR_ENVELOPES',
  };
};

export const movePoint = (id, envelopeId, position) => {
  return {
    type: "MOVE_POINT",
    envelopeId: envelopeId,
    id: id,
    position: position,
  };
};

export const swapPoints = (id, swapId, envelopeId) => {
  return {
    type: "SWAP_POINTS",
    id,
    swapId,
    envelopeId,
  };
};

export const changeColor = (color) => {
return {
    type: 'CHANGE_COLOR',
    color,
  };
};

export const toggleActiveEnvelope = (id) => {
  return {
    type: "TOGGLE_ACTIVE_ENVELOPE",
    id,
  };
};

export const toggleActivePoint = (envelopeId, pointId) => {
  return {
    type: "TOGGLE_ACTIVE_POINT",
    envelopeId,
    pointId,
  };
};
