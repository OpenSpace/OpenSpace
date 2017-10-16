const point = (state={}, action, id) => {
  switch(action.type) {
    case 'ADD_ENVELOPE':
      return {
        id: id,
        position: action.positions[id],
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

const histogram = (state={}, action) => {  // state refers to individual envelope
  switch(action.type) {
    case 'ADD_HISTOGRAM':
      let counter = 0;
      let points = action.positions.map(position =>
          point(undefined, action, counter++),
        );
      return {
        id: action.id,
        points: points,
        color: action.color,
        active: false,
      }
    case 'CHANGE_COLOR':
        return state.map(envelope =>
          (histogram.active === true)
          ? Object.assign({}, state, {
            color: action.color
          })
          : histogram
          )
    default:
      return state;
  }
};

export const histograms = (state=[], action) => {  // state refers to array of histograms
  switch(action.type) {
    case 'ADD_HISTOGRAM':
      return [histogram(undefined, action)];
    default:
      return state;
  }
};
export default histograms;
