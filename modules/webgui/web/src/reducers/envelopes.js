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
        anchor: anchor,
        color: value.color,
      }
    default:
      return state;
  }
}

const envelope = (state={}, action) => {  // state refers to individual envelope
  switch(action.type) {
    case 'ADD_ENVELOPE':
      let counter = 0;
      let points = action.points.map((value) =>
          point(undefined, action, value, counter++),
        );
      return {
        id: action.id,
        points: points,
        active: false,
        size: 4,
      }
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
      return state.filter(envelope => envelope.active !== true)
    case 'MOVE_POINT':
      return  state.map(envelope => ({
              ...envelope,
              points: envelope.points.map(point => ({
                ...point,
                position: (point.id === action.id) ?
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
