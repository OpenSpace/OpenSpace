let IdContainer = [];
export const addEnvelope = (points, transferfunctionId) => {
  console.log(points)
  return {
    type: 'ADD_ENVELOPE',
    transferfunctionId,
    id: IdContainer[transferfunctionId]++,
    points,
  };
};

export const addPoint = (color, transferfunctionId) => {
  return {
    type: 'ADD_POINT',
    transferfunctionId,
    color,
  };
};

export const deleteEnvelope = (transferfunctionId) => {
  console.log(transferfunctionId)
  return {
    type: 'DELETE_ENVELOPE',
    transferfunctionId,
  };
};

export const clearEnvelopes = (transferfunctionId) => {
  return {
    type: 'CLEAR_ENVELOPES',
    transferfunctionId,
  };
};

export const movePoint = (id, envelopeId, position, transferfunctionId) => {
  return {
    type: "MOVE_POINT",
    envelopeId: envelopeId,
    id: id,
    position: position,
    transferfunctionId,
  };
};

export const swapPoints = (id, swapId, envelopeId, transferfunctionId) => {
  return {
    type: "SWAP_POINTS",
    id,
    swapId,
    envelopeId,
    transferfunctionId,
  };
};

export const changeColor = (color, transferfunctionId) => {
return {
    type: 'CHANGE_COLOR',
    color,
    transferfunctionId,
  };
};

export const toggleActiveEnvelope = (id, transferfunctionId) => {
  return {
    type: "TOGGLE_ACTIVE_ENVELOPE",
    id,
    transferfunctionId,
  };
};

export const toggleActivePoint = (envelopeId, pointId, transferfunctionId) => {
  return {
    type: "TOGGLE_ACTIVE_POINT",
    envelopeId,
    pointId,
    transferfunctionId,
  };
};

export const addTransferFunction = (transferfunction) => {
  IdContainer[transferfunction.id] = 0;
  return {
    type: "ADD_TRANSFER_FUNCTION",
    transferfunction,
  };
};
