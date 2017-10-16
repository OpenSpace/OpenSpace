let nextEnvelopeId = 0;
let nextHistogramId = 0;
export const addEnvelope = (positions, color) => {
  return {
    type: 'ADD_ENVELOPE',
    id: nextEnvelopeId++,
    positions: positions,
    color,
  };
};

export const deleteEnvelope = () => {
  return {
    type: 'DELETE_ENVELOPE',
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

export const addHistogram = (positions, color) => {
return {
    type: 'ADD_HISTOGRAM',
    id: nextHistogramId++,
    positions: positions,
    color,
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
