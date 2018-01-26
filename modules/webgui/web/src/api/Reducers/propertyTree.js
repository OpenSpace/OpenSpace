import * as helperFunctions from '../../utils/propertyTreeHelpers';
import envelopes from './envelopes';
import { actionTypes } from '../Actions/actionTypes';


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
        name: action.payload.node.name,
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
      if (action.payload.property.Description.Type === 'TransferFunctionProperty') {
        return envelopes(undefined, action);
      }

      return action.payload.property.Value;

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
        id: helperFunctions.getIdOfProperty(action.payload.property.Description.Identifier),
        Description: action.payload.property.Description,
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
        const newAction = {
          ...action,
          payload: {
            property: element,
          },
        };
        return property(undefined, newAction);
      });
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode:
    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
      return state.map((element) => {
        if (element.id === action.payload.name) {
          return property(element, action);
        }
        return element;
      });
    case actionTypes.removeNode: {
      return state.map((element) => {
        if (!(element.id === action.payload.name)) {
          return element;
        }
      });
    }
    default:
      return state;
  }
};

const propertyOwner = (state = {}, action) => { // state refers to a single node
  switch (action.type) {
    case actionTypes.insertNode:
      return {
        name: action.payload.node.name,
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
    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.newURI);
      if (action.payload.lastNode) {
        return changePropertyOwnerState(state, action);
      }
      const newAction = {
        ...action,
        payload: {
          ...action.payload,
          newURI: splittedURI.URI,
          name: splittedURI.name,
          lastNode: splittedURI.lastNode,
        },
      };
      return {
        ...state,
        subowners: state.subowners.map((element) => {
          if (element.name === splittedURI.name) {
            return propertyOwner(element, newAction);
          }
          return element;
        }),
        properties: properties(state.properties, newAction),
      };
    }
    case actionTypes.removeNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      const newAction = {
        ...action,
        payload: {
          ...action.payload,
          newURI: splittedURI.URI,
          name: splittedURI.name,
          lastNode: splittedURI.lastNode,
        },
      };
      return {
        ...state,
        subowners: state.subowners.map((element) => {
          if (element.name === splittedURI.name) {
            if (!splittedURI.lastNode) {
              return propertyOwner(element, newAction);
            }
          } else {
            return element;
          }
        }),
        properties: (splittedURI.lastOwner) ?
          properties(state.properties, newAction) :
          state.properties,
      };
    }
    case actionTypes.addNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.newURI);
      if (action.payload.lastOwner) {
        if (action.payload.subowners === undefined) {
          return {
            ...state,
            properties: properties(state.properties, action),
          };
        }

        const newAction = {
          type: actionTypes.insertNode,
          payload: {
            ...action.payload,
            newURI: splittedURI.URI,
            name: splittedURI.name,
            lastNode: splittedURI.lastNode,
          },
        };
        return {
          ...state,
          subowners: [
            ...state.subowners,
            propertyOwner(undefined, newAction),
          ],
        };
      }

      const newAction = {
        ...action,
        payload: {
          ...action.payload,
          newURI: splittedURI.URI,
          name: splittedURI.name,
          lastNode: splittedURI.lastNode,
        },
      };
      return {
        ...state,
        subowners: state.subowners.map((element) => {
          if (element.name === splittedURI.name) {
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

export const propertyTree = (state = [], action) => { // state refers to all "root" nodes
  switch (action.type) {
    case actionTypes.initializePropertyTree: {
      const itemsToInsert = action.payload.data.map((node) => {
        const newAction = {
          type: actionTypes.insertNode,
          payload: {
            node,
          },
        };
        return propertyOwner(undefined, newAction);
      });
      return state.concat(itemsToInsert);
    }
    case actionTypes.startListeningToNode:
    case actionTypes.stopListeningToNode:
    case actionTypes.updatePropertyTreeNode:
    case actionTypes.changePropertyTreeNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      return state.map((element) => {
        if (element.name === splittedURI.name) {
          const newAction = {
            ...action,
            payload: {
              ...action.payload,
              newURI: splittedURI.URI,
              name: splittedURI.name,
              lastNode: splittedURI.lastNode,
            },
          };
          return propertyOwner(element, newAction);
        }
        return element;
      });
    }
    case actionTypes.removeNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      return state.map((element) => {
        if (element.name === splittedURI.name) {
          if (!splittedURI.lastNode) {
            const newAction = {
              type: actionTypes.insertNode,
              payload: {
                ...action.payload,
                newURI: splittedURI.URI,
                name: splittedURI.name,
                lastNode: splittedURI.lastNode,
              },
            };
            return propertyOwner(element, newAction);
          }
        } else {
          return element;
        }
      });
    }
    case actionTypes.addNode: {
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      return state.map((element) => {
        if (element.name === splittedURI.name) {
          if (splittedURI.lastNode) {
            return [...state, propertyOwner(undefined, action)];
          }
          return state.map((element) => {
            if (element.name === splittedURI.name) {
              const newAction = {
                ...action,
                payload: {
                  ...action.payload,
                  newURI: splittedURI.URI,
                  name: splittedURI.name,
                  lastNode: splittedURI.lastNode,
                },
              };
              return propertyOwner(element, newAction);
            }
            return element;
          });
        }
        return element;
      });
    }
    default:
      return state;
  }
};
