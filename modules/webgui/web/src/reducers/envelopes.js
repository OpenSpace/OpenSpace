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
    case 'MOVE_POINT':
       return state.map(envelope =>
        (envelope.id === action.envelopeId)
          ? Object.assign({}, envelope, {
            points: envelope.points.map(point =>
              (point.id === action.id)
              ? Object.assign({}, point, {
                position: action.position,
                })
              : point),
          })
          : envelope
      )
    case 'DELETE_ENVELOPE':
        return state.filter(envelope => envelope.active !== true)
    case 'CHANGE_COLOR':
      return state.map(envelope =>
          Object.assign({}, envelope, {
            points: envelope.points.map(point =>
            (point.active || envelope.active)
            ? Object.assign({}, point, {
                color: action.color,
                })
            : point),
          })
        )
    case 'TOGGLE_ACTIVE_ENVELOPE':
      return state.map(envelope =>
          (envelope.id === action.id)
          ? Object.assign({}, envelope, {
            points: envelope.points.map(point =>
              Object.assign({}, point, {
                active: false,
              })
            ),
            active: !envelope.active,
          })
          : Object.assign({}, envelope, {
            points: envelope.points.map(point =>
              Object.assign({}, point, {
                active: false,
              })
            ),
            active: false,
          })
        )
    case 'TOGGLE_ACTIVE_POINT':
      return state.map(envelope =>
          (envelope.id === action.envelopeId)
          ? Object.assign({}, envelope, {
            points: envelope.points.map(point =>
              (point.id === action.pointId)
              ? Object.assign({}, point, {
                active: !point.active,
                })
              : Object.assign({}, point, {
                active: false,
                })
              ),
              active: false,
          })
          : Object.assign({}, envelope, {
            points: envelope.points.map(point =>
              Object.assign({}, point, {
                active: false,
                })
              ),
            active: false,
          })
        )
    default:
      return state;
  }
};

export default envelopes;
