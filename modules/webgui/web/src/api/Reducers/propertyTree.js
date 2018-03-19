import * as helperFunctions from '../../utils/propertyTreeHelpers';
import envelopes from './envelopes';
import { actionTypes } from '../Actions/actionTypes';

const updateActionURI = (action, splittedURI) => {
  return {
    ...action,
    payload: {
      ...action.payload,
      URI: splittedURI.URI,
      identifier: splittedURI.identifier,
      isLastNode: splittedURI.isLastNode,
    },
  };
};

const updateActionNode = (action, node) => {
  return {
    ...action,
    payload: {
      node,
    },
  };
};

const updateActionType = (action, type) => {
  return {
    ...action,
    type,
  };
};

const changePropertyOwnerState = (state, action) => {
  switch (action.type) {
    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
      return {
        ...state,
        listeners: (action.type === actionTypes.startListeningToNode) ?
          state.listeners + 1 : state.listeners - 1,
      };
    case actionTypes.updatePropertyTreeNode:
      return {
        identifier: action.payload.node.identifier,
        properties: properties(undefined, action),
        subowners: action.payload.node.subowners.map((subowner) => {
          const newAction = {
            ...action,
            payload: {
              node: subowner,
            },
          };
          return propertyOwner(undefined, newAction);
        }),
        tag: (action.payload.node.tag === undefined) ? [] : action.payload.node.tag,
        listeners: 0,
      };
    default:
      return state;
  }
};

export const setPropertyValue = (state, action) => {
  switch (action.type) {
    case actionTypes.insertNode:
      if (action.payload.node.Description.Type === 'TransferFunctionProperty') {
        return envelopes(undefined, action);
      }
      return action.payload.node.Value;
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode:
      if (state.Description.Type === 'TransferFunctionProperty') {
        return envelopes(state.Value, action);
      }
      return action.payload.value;
    default:
      return action.payload.Value;
  }
};

const property = (state = {}, action) => { // state refers to a single property
  switch (action.type) {
    case actionTypes.insertNode:
      return {
        id: helperFunctions.getIdOfProperty(action.payload.node.Description.Identifier),
        Description: action.payload.node.Description,
        Value: setPropertyValue(undefined, action),
        listeners: 0,
      };
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode:
      return {
        ...state,
        Value: setPropertyValue(state, action),
      };
    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
      return {
        ...state,
        listeners: (action.type === actionTypes.startListeningToNode) ?
          state.listeners + 1 : state.listeners - 1,
      };
    default:
      return state;
  }
};

const properties = (state = [], action) => { // state refers to an array of properties
  switch (action.type) {
    case actionTypes.insertNode:
      return action.payload.node.properties.map((element) => {
        return property(undefined, updateActionNode(action, element));
      });
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode:
    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
      return state.map((element) => {
        if (element.id === action.payload.identifier) {
          return property(element, action);
        }
        return element;
      });
    case actionTypes.removeNode: {
      return state.map((element) => {
        if (!(element.id === action.payload.identifier)) {
          return element;
        }
      });
    }
    default:
      return state;
  }
};

export const propertyOwner = (state = {}, action) => { // state refers to a single node
  switch (action.type) {
    case actionTypes.insertNode:
      return {
        identifier: action.payload.node.identifier,
        properties: properties(undefined, action),
        subowners: action.payload.node.subowners.map((subowner) => {
          return propertyOwner(undefined, updateActionNode(action, subowner));
        }),
        tag: (action.payload.node.tag === undefined) ? [] : action.payload.node.tag,
        listeners: 0,
      };

    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      if (action.payload.isLastNode) {
        return changePropertyOwnerState(state, action);
      }
      const newAction = updateActionURI(action, splittedURI);
      return {
        ...state,
        subowners: state.subowners.map((element) => {
          if (element.identifier === splittedURI.identifier) {
            return propertyOwner(element, newAction);
          }
          return element;
        }),
        properties: properties(state.properties, newAction),
      };
    }

    case actionTypes.removeNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      const newAction = updateActionURI(action, splittedURI);
      return {
        ...state,
        subowners: state.subowners.map((element) => {
          if (element.identifier === splittedURI.identifier) {
            if (!splittedURI.isLastNode) {
              return propertyOwner(element, newAction);
            }
          } else {
            return element;
          }
        }),
        properties: (splittedURI.isLastOwner) ?
          properties(state.properties, newAction) :
          state.properties,
      };
    }

    case actionTypes.addNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      if (action.payload.isLastOwner) {
        if (action.payload.subowners === undefined) {
          return {
            ...state,
            properties: properties(state.properties, action),
          };
        }
        return {
          ...state,
          subowners: [
            ...state.subowners,
            propertyOwner(undefined, updateActionType( action, actionTypes.insertNode) ), 
          ],
        };
      }
      const newAction = getNewNodeAction(action, splittedURI);
      return {
        ...state,
        subowners: state.subowners.map((element) => {
          if (element.identifier === splittedURI.identifier) {
            return propertyOwner(element, newAction);
          }
          return element;
        }),
      };
    }
    default:
      return state;
  }
};