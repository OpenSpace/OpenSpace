const point = (state={}, action, value, id) => {
  switch(action.type) {
    case 'ADD_ENVELOPE':
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
    case 'ADD_POINT':
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
  switch(action.type) {
    case 'ADD_TRANSFER_FUNCTION':
      return [...state];
    case 'ADD_ENVELOPE':
      console.log(action)
      let counter = 0;
      let points = action.points.map((value) =>
          point(undefined, action, value, counter++),
        );
      return {
        id: action.id,
        points: points,
        active: false,
      }
    case 'ADD_POINT':
      var pointId = Math.ceil(state.points.length / 2);
      let pointValue = Object.assign({},
          { position: {
            x: (state.points[pointId - 1].position.x + state.points[pointId].position.x) / 2,
            y: (state.points[pointId - 1].position.y + state.points[pointId].position.y) / 2,
          },
          color: action.color
        })
      //FEL SKA FIXAS
      state.points.splice(pointId, 0, point(undefined, action, pointValue, pointId));
      return {...state, id: action.id, points: state.points.map((point, index) => ({
              ...point,
              id: index,
             }))};
    case 'SWAP_POINTS':
      state.points.sort(function(a, b) {
        return a.position.x - b.position.x;
      });
      return {...state};
    default:
      return state;
  }
};

const envelopes = (state=[], action) => {  // state refers to array of envelopes
  switch(action.type) {
    case 'ADD_ENVELOPE':
      return [...state, envelope(undefined, action)];
    case 'CLEAR_ENVELOPES':
      return [];
    case 'DELETE_ENVELOPE':
      return state.filter(envelope => envelope.active !== true);
    case 'ADD_POINT':
      return state.map(value => {
            if(value.active === true) {
              return envelope(value, action);
            }
            else {
             return value;
            }
           });
    case 'SWAP_POINTS':
      return  state.map(value => {
                if(value.id === action.envelopeId) {
                  return envelope(value, action);
                }
                else {
                 return value;
                }
              });
    case 'MOVE_POINT':
      return  state.map(envelope => ({
              ...envelope,
              points: envelope.points.map(point => ({
                ...point,
                position: (point.id === action.id && envelope.id === action.envelopeId) ?
                  action.position
                : point.position })),
                })
              );
    case 'CHANGE_COLOR':
      return state.map(envelope => ({
              ...envelope,
              points: envelope.points.map(point => ({
                ...point,
                color: (point.active || envelope.active) ?
                  action.color
                : point.color })),
                })
              );
    case 'TOGGLE_ACTIVE_ENVELOPE':
      return state.map(envelope => ({
              ...envelope,
              active: (envelope.id === action.id) ?
                !envelope.active
                : false,
              points: envelope.points.map(point => ({
                ...point,
                active: false
                })),
              })
            );
    case 'TOGGLE_ACTIVE_POINT':
      return state.map(envelope => ({
              ...envelope,
              active: false,
              points: envelope.points.map(point => ({
                ...point,
                active: (point.id === action.pointId &&
                  envelope.id === action.envelopeId) ?
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
