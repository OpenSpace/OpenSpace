import * as helperFunctions from './sceneGraphHelperFunctions.js'

const property = (state={}, action) => {  // state refers to a single property
  switch(action.type) {
    case 'PROPERTYOWNER_ADD_OWNER':
      return {
        id: helperFunctions.getIdOfProperty(action.payload.property.Description.Identifier),
        Description: action.payload.property.Description,
        Value: helperFunctions.setPropertyValue(undefined, action),
        listeners: 0,
      }
    case 'SCENEGRAPH_UPDATE_PROPERTY':
    case 'SCENEGRAPH_CHANGE_PROPERTY':
      return {
        ...state,
        Value: helperFunctions.setPropertyValue(state, action)
      }
    case 'SCENEGRAPH_START_LISTENING':
    case 'SCENEGRAPH_STOP_LISTENING':
      return {
        ...state,
        listeners: (action.type == 'SCENEGRAPH_START_LISTENING') ? 
                    state.listeners + 1 : state.listeners - 1,
      }
    default:
      return state;
  }
};

const properties = (state=[], action) => {  // state refers to an array of properties
  switch(action.type) {
    case 'PROPERTYOWNER_ADD_OWNER':
      return action.payload.node.properties.map(element => {
         const newAction = {
            ...action,
            payload: {
              property: element
            }
          }
          return property(undefined, newAction);
      })
    case 'SCENEGRAPH_UPDATE_PROPERTY':
    case 'SCENEGRAPH_CHANGE_PROPERTY':
    case 'SCENEGRAPH_START_LISTENING':
    case 'SCENEGRAPH_STOP_LISTENING':
      return state.map(element => {
        if (element.id === action.payload.name) {
          return property(element, action);
        }
        else {
          return element;
        }
      })
    default:
      return state;
  }
};

const propertyOwner = (state={}, action) => { // state refers to a single node
  switch(action.type) {
    case 'PROPERTYOWNER_ADD_OWNER':
      return {
        name: action.payload.node.name,
        properties: properties(undefined, action),
        subowners: action.payload.node.subowners.map(subowner => {
          const newAction = {
            ...action,
            payload: {
              node: subowner
            }
          }
          return propertyOwner(undefined, newAction);
        }),
        tag:(action.payload.node.tag === undefined) ? [] : action.payload.node.tag,
        listeners: 0,
      }
    case 'SCENEGRAPH_START_LISTENING':
    case 'SCENEGRAPH_STOP_LISTENING':
    case 'SCENEGRAPH_UPDATE_PROPERTY':
    case 'SCENEGRAPH_CHANGE_PROPERTY':
        const splittedURI = helperFunctions.splitURI(action.payload.newURI);
        if (splittedURI.lastNode) {
          return helperFunctions.changePropertyOwnerState(state, action);
        }
        else {
          const newAction = {
            ...action,
            payload: {
              ...action.payload,
              newURI: splittedURI.URI,
              name: splittedURI.name
            }
          }
          return {
            ...state,
            subowners: state.subowners.map(element => {
              if (element.name === splittedURI.name) {
                return propertyOwner(element, newAction);
              }
              else {
                return element;
              }
            }),
            properties: properties(state.properties, newAction)
          }
        }
    default:
      return state;
  }
};

export const sceneGraph = (state=[], action) => {  // state refers to all "root" nodes
  switch(action.type) {
    case 'SCENEGRAPH_INSERT':
      let itemsToInsert = action.payload.data.map(node => {
          const newAction = {
            type: 'PROPERTYOWNER_ADD_OWNER',
            payload: {
              node,
            }
          }
          return propertyOwner(undefined, newAction);
      });
      return state.concat(itemsToInsert);
    case 'SCENEGRAPH_START_LISTENING':
    case 'SCENEGRAPH_STOP_LISTENING':
    case 'SCENEGRAPH_UPDATE_PROPERTY':
    case 'SCENEGRAPH_CHANGE_PROPERTY':
      const splittedURI = helperFunctions.splitURI(action.payload.URI);
      return state.map(element => {
        if (element.name === splittedURI.name) {
          const newAction = {
            ...action,
            payload: {
              ...action.payload,
              newURI: splittedURI.URI,
              name: splittedURI.name
            }
          }
          return propertyOwner(element, newAction);
        }
        else {
          return element;
        }
      })
    default:
      return state;
  }
};