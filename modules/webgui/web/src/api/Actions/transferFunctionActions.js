let IdContainer = [];

export const addEnvelope = (points, URI) => {
  if (!(URI in IdContainer)) {
    IdContainer[URI] = 0;
  }
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_ADD_ENVELOPE',
      id: IdContainer[URI]++,
      points,
      URI,
    }
  };
};

export const addPoint = (color, URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_ADD_POINT',
      color,
      URI,
    }
  };
};

export const deleteEnvelope = (URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_DELETE_ENVELOPE',
      URI,
    }
  };
};

export const clearEnvelopes = (URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_CLEAR_ENVELOPES',
      URI,
    }
  };
};

export const movePoint = (id, envelopeId, position, URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_MOVE_POINT',
      envelopeId: envelopeId,
      id: id,
      position: position,
      URI,
    }
  };
};

export const swapPoints = (id, swapId, envelopeId, URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_SWAP_POINTS',
      id,
      swapId,
      envelopeId,
      URI,
    }
  };
};

export const changeColor = (color, URI) => {
return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_CHANGE_COLOR',
      color,
      URI,
    }
  };
};

export const toggleActiveEnvelope = (id, URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_TOGGLE_ACTIVE_ENVELOPE',
      id,
      URI,
    }
  };
};

export const toggleActivePoint = (envelopeId, pointId, URI) => {
  return {
    type: 'SCENEGRAPH_CHANGE_PROPERTY',
    payload: {
      type: 'TRANSFERFUNCTION_TOGGLE_ACTIVE_POINT',
      envelopeId,
      pointId,
      URI,
    }
  };
};