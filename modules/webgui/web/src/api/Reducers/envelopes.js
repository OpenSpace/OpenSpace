import { actionTypes } from '../Actions/actionTypes';

const point = (state = {}, action, value, id) => {
  switch (action.payload.type) {
    case actionTypes.addEnvelope: {
      let anchor = false;
      if (id === 0 || id === 3) {
        anchor = true;
      }
      return {
        id,
        position: value.position,
        active: false,
        clickable: true,
        anchor,
        color: value.color,
      };
    }
    case actionTypes.addPoint: {
      return {
        id,
        position: value.position,
        active: false,
        clickable: true,
        anchor: false,
        color: value.color,
      };
    }
    default:
      return state;
  }
};

/*
  Envelopes are just an arbitrary amount of points that can be moved in the TF-editor
*/
const envelope = (state = {}, action) => { // state refers to individual envelope
  switch (action.payload.type) {
    case actionTypes.addTransferFunction:
      return [...state];
    case actionTypes.addEnvelope: {
      let counter = 0;
      const points = action.payload.points.map(value =>
        point(undefined, action, value, counter++),
      );
      return {
        id: action.payload.id,
        points,
        active: false,
      };
    }
    case actionTypes.addPoint: {
      const pointPos = Math.ceil(state.points.length / 2);
      const pointId = state.points.length;
      const pointValue = Object.assign({},
        {
          position: {
            x: (state.points[pointPos - 1].position.x + state.points[pointPos].position.x) / 2,
            y: (state.points[pointPos - 1].position.y + state.points[pointPos].position.y) / 2,
          },
          color: action.payload.color,
        });
      // TODO Clean up
      state.points.splice(pointPos, 0, point(undefined, action, pointValue, pointId));
      return {
        ...state,
        points: state.points.map((point) => ({
          ...point,
        })),
      };
    }
    case actionTypes.movePoint: {
      const points = state.points.map(point => ({
        ...point,
        position: (point.id === action.payload.id) ?
          {
            x: point.position.x + action.payload.deltaPosition.x,
            y: point.position.y + action.payload.deltaPosition.y,
          }
          : point.position,
      }));
      const swapPoints = points
        .slice(1, points.length - 1).sort((a, b) => a.position.x - b.position.x);

      swapPoints.splice(0, 0, points[0]);
      swapPoints.splice(points.length - 1, 0, points[points.length - 1]);
      return {
        ...state,
        points: swapPoints,
      };
    }
    case actionTypes.changeColor:
      return {
        ...state,
        points: state.points.map(point => ({
          ...point,
          color: (point.active || state.active) ?
            action.payload.color
            : point.color })),
      };
    case actionTypes.toggleActiveEnvelope:
      return {
        ...state,
        active: (state.id === action.payload.envelopeId) ?
          !state.active
          : false,
        points: state.points.map(point => ({
          ...point,
          active: false,
        })),
      };
    case actionTypes.toggleActivePoint:
      return {
        ...state,
        active: false,
        points: state.points.map(point => ({
          ...point,
          active: (point.id === action.payload.pointId) ?
            !point.active
            : false,
        })),
      };
    case actionTypes.setClickablePoint:
      return {
        ...state,
        points: state.points.map(point => ({
          ...point,
          clickable: (point.id === action.payload.pointId) ?
            action.payload.isClickable
            : true,
        })),
      };
    default:
      return state;
  }
};

const envelopes = (state = [], action) => { // state refers to array of envelopes
  switch (action.payload.type) {
    case actionTypes.addEnvelope:
      return [...state, envelope(undefined, action)];
    case actionTypes.clearEnvelopes:
      return [];
    case actionTypes.deleteEnvelope:
      return state.filter(envelope => envelope.active !== true);
    case actionTypes.addPoint:
      return state.map((element) => {
        if (element.active === true) {
          return envelope(element, action);
        }
        return element;
      });
    case actionTypes.movePoint:
    case actionTypes.toggleActivePoint:
    case actionTypes.setClickablePoint:
      return state.map((element) => {
        if (action.payload.envelopeId === element.id) {
          return envelope(element, action);
        }
        return element;
      });
    case actionTypes.toggleActiveEnvelope:
    case actionTypes.changeColor:
      return state.map(element => envelope(element, action));
    default:
      return state;
  }
};

export default envelopes;
