import {actionTypes} from "../Actions/actionTypes";

const info = (state = {}, action) => {
  switch(action.type){
    case actionTypes.fetchData:
      return {
        id: action.payload.id,
        fetching: true,
        succeed: false,
      };
    case actionTypes.fetchDataDone:
      return {
        ...state,
        data: action.payload.data,
        fetching: false,
        succeed: true,
      };
    case actionTypes.fetchDataFailed:
        return{
          id: action.payload.id,
          fetching: false,
          succeed: false,
        };
    default:
      return state;
  }
};

export const fetchData = (state=[], action) => {
  switch(action.type) {
    case actionTypes.fetchData:
    return [...state, info(undefined,action)];
    case actionTypes.fetchDataDone:
      return(state.map((element) => {
        if(action.payload.id === element.id) {
          return info(element, action);
        }
      }));
    case actionTypes.fetchDataFailed:
     return state.map((element) => {
        if(!element){
         return info(element, action);
       }
        return element;
      });
    default:
      return state;
  }
};
