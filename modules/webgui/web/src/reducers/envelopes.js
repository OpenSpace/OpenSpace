
const point = (state={}, action, id) => {
  switch(action.type) {
    case 'ADD_ENVELOPE':
      let anchor = false;
      if (id === 0 || id === 3)
        anchor = true;
      return {
        id: id,
        position: action.positions[id],
        anchor: anchor,
      }
    case 'ADD_HISTOGRAM':
      return {
        id: id,
        position: action.positions[id],
      }
    default:
      return state;
  }
}

const envelope = (state={}, action) => {  // state refers to individual envelope
  switch(action.type) {
    case 'ADD_ENVELOPE':
      let counter = 0;
      let points = action.positions.map(position =>
          point(undefined, action, counter++),
        );
      return {
        id: action.id,
        points: points,
        color: action.color,
        active: true,
      }
    default:
      return state;
  }
};

const envelopes = (state=[], action) => {  // state refers to array of envelopes
  switch(action.type) {
    case 'ADD_ENVELOPE':
      return [...state, envelope(undefined, action)];
    case 'MOVE_POINT':
       return state.map(envelope =>
        (envelope.id === action.envelopeId)
          ? Object.assign({}, envelope, {
            points: envelope.points.map(point =>
              (point.id === action.id)
              ? Object.assign({}, point, {
                position: action.position,
                id: action.id,
                })
              : point),
          })
          : envelope
      )
    case 'DELETE_ENVELOPE':
        return state.filter(envelope => envelope.active !== true)
    case 'CHANGE_COLOR':
      return state.map(envelope =>
          (envelope.active === true)
          ? Object.assign({}, envelope, {
            color: action.color
          })
          : envelope
        )
    case 'TOGGLE_ACTIVE_ENVELOPE':
      return state.map(envelope =>
          (envelope.id === action.id)
          ? Object.assign({}, envelope, {
            active: !envelope.active,
          })
          : envelope
        )
    default:
      return state;
  }
};

export default envelopes;
