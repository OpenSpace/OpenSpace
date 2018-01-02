const point = (state={}, action, value, id) => {
  switch(action.payload.type) {
    case 'TRANSFERFUNCTION_ADD_ENVELOPE':
      let anchor = false;
      if (id === 0 || id === 3)
        anchor = true;
      return {
        id: id,
        position: value.position,
        active: false,
        clickable: true,
        anchor: anchor,
        color: value.color,
      }
    case 'TRANSFERFUNCTION_ADD_POINT':
      return {
        id: id,
        position: value.position,
        active: false,
        clickable: true,
        anchor: false,
        color: value.color,
      }
    default:
      return state;
  }
}

const envelope = (state={}, action) => {  // state refers to individual envelope
  switch(action.payload.type) {
    case 'TRANSFERFUNCTION_ADD_TRANSFER_FUNCTION':
      return [...state];
    case 'TRANSFERFUNCTION_ADD_ENVELOPE':
      let counter = 0;
      let points = action.payload.points.map((value) =>
          point(undefined, action, value, counter++),
        );
      return {
        id: action.payload.id,
        points: points,
        active: false,
      }
    case 'TRANSFERFUNCTION_ADD_POINT':
      var pointPos = Math.ceil(state.points.length / 2);
      var pointId = state.points.length;
      let pointValue = Object.assign({},
          { position: {
            x: (state.points[pointPos - 1].position.x + state.points[pointPos].position.x) / 2,
            y: (state.points[pointPos - 1].position.y + state.points[pointPos].position.y) / 2,
          },
          color: action.payload.color
        })
      //FEL SKA FIXAS
      state.points.splice(pointPos, 0, point(undefined, action, pointValue, pointId));
      return {...state, points: state.points.map((point, index) => ({
              ...point,
             }))};
    case 'TRANSFERFUNCTION_SWAP_POINTS':
      const results = state.points.slice();
      const firstItem = state.points[action.payload.id];
      results[action.payload.id] = state.points[action.payload.swapId];
      results[action.payload.swapId] = firstItem;
      return {...state, points: results};
    default:
      return state;
  }
};

const envelopes = (state=[], action) => {  // state refers to array of envelopes
  switch(action.payload.type) {
    case 'TRANSFERFUNCTION_ADD_ENVELOPE':
      return [...state, envelope(undefined, action)];
    case 'TRANSFERFUNCTION_CLEAR_ENVELOPES':
      return [];
    case 'TRANSFERFUNCTION_DELETE_ENVELOPE':
      return state.filter(envelope => envelope.active !== true);
    case 'TRANSFERFUNCTION_ADD_POINT':
      return state.map(value => {
            if(value.active === true) {
              return envelope(value, action);
            }
            else {
             return value;
            }
           });
    case 'TRANSFERFUNCTION_SWAP_POINTS':
      return  state.map(value => {
                if(value.id === action.payload.envelopeId) {
                  return envelope(value, action);
                }
                else {
                 return value;
                }
              });
    case 'TRANSFERFUNCTION_MOVE_POINT':
      return  state.map(envelope => ({
              ...envelope,
              points: envelope.points.map(point => ({
                ...point,
                position: (point.id === action.payload.id && envelope.id === action.payload.envelopeId) ?
                  action.payload.position
                : point.position })),
                })
              );
    case 'TRANSFERFUNCTION_CHANGE_COLOR':
      return state.map(envelope => ({
              ...envelope,
              points: envelope.points.map(point => ({
                ...point,
                color: (point.active || envelope.active) ?
                  action.payload.color
                : point.color })),
                })
              );
    case 'TRANSFERFUNCTION_TOGGLE_ACTIVE_ENVELOPE':
      return state.map(envelope => ({
              ...envelope,
              active: (envelope.id === action.payload.id) ?
                !envelope.active
                : false,
              points: envelope.points.map(point => ({
                ...point,
                active: false
                })),
              })
            );
    case 'TRANSFERFUNCTION_TOGGLE_ACTIVE_POINT':
      return state.map(envelope => ({
              ...envelope,
              active: false,
              points: envelope.points.map(point => ({
                ...point,
                active: (point.id === action.payload.pointId &&
                  envelope.id === action.payload.envelopeId) ?
                  !point.active
                  : false,
                })),
              })
            );
    default:
      return state;
  }
};

export default envelopes;
