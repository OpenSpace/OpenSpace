import envelopes from './envelopes.js'

const transferfunction = (state={}, action) => {
  switch(action.type) {
    case 'ADD_TRANSFER_FUNCTION':
      return {
        ...action.transferfunction,
        data: {
          ...action.transferfunction.data,
          TransferFunction: {
            ...action.transferfunction.data.TransferFunction,
            envelopes: envelopes(undefined, action)
          }
        }
      };
      break;
    case 'ADD_ENVELOPE':
    case 'CLEAR_ENVELOPES':
    case 'DELETE_ENVELOPE':
    case 'SWAP_POINTS':
    case 'MOVE_POINT':
    case 'CHANGE_COLOR':
    case 'TOGGLE_ACTIVE_ENVELOPE':
    case 'TOGGLE_ACTIVE_POINT':
      return {
        ...state,
        data: {
          ...state.data,
          TransferFunction: {
            ...state.data.TransferFunction,
            envelopes: envelopes(state.data.TransferFunction.envelopes, action)
          }
        }
      };
      break;
    default:
      return state;
  }
};

const transferfunctions = (state=[], action) => {  // state refers to array of transferfunctions
  switch(action.type) {
    case 'ADD_TRANSFER_FUNCTION':
      var transferfunctionExists = false;
      state.forEach(function(element){
        if(element.id === action.transferfunction.id) {
          transferfunctionExists = true;
        }
      })
      if (transferfunctionExists) {
        return [...state];
      }
      else {
        return [...state, transferfunction(undefined, action)];
      }
      break;
    case 'ADD_ENVELOPE':
    case 'CLEAR_ENVELOPES':
    case 'DELETE_ENVELOPE':
    case 'SWAP_POINTS':
    case 'MOVE_POINT':
    case 'CHANGE_COLOR':
    case 'TOGGLE_ACTIVE_ENVELOPE':
    case 'TOGGLE_ACTIVE_POINT':
    console.log(action)
      return state.map(element => {
        if (element.id === action.transferfunctionId) {
          return transferfunction(element, action);
        }
        else {
          return element;
        }
      })
    default:
      return state;
  }
};

export default transferfunctions;
