import envelopes from './envelopes.js'

export const setPropertyValue = (state, action) => {
  switch(action.type) {
    case 'PROPERTYOWNER_ADD_OWNER':
        if (action.payload.property.Description.Type === "TransferFunctionProperty") {
          return envelopes(undefined, action);
        }
        else {
          return action.payload.property.Value;
        }
    case 'SCENEGRAPH_UPDATE_PROPERTY':
    case 'SCENEGRAPH_CHANGE_PROPERTY':
        if (state.Description.Type === "TransferFunctionProperty") {
          return envelopes(state.Value, action);
        }
        else {
          return action.payload.value;
        }
    default:
      return action.payload.Value;
  }
}

export const splitURI = (URI) => {
  const indexForName = URI.indexOf('.');
  const name = URI.substring(0, indexForName != -1 ?
    indexForName : URI.length);
  URI = URI.substring(indexForName + 1, URI.length);

  return {name: name, URI: URI, lastOwner: URI.indexOf('.') === -1, lastNode: URI.length === 0}
}

export const getIdOfProperty = (URI) => {
  const indexForName = URI.lastIndexOf('.');
  return URI.substring(indexForName != -1 ?
    indexForName + 1 : 0, URI.length);

}

export const changePropertyOwnerState = (state, action) => {
    switch(action.type) {
      case 'SCENEGRAPH_START_LISTENING':
      case 'SCENEGRAPH_START_LISTENING':
        return {
          ...state,
          listeners: (action.type == 'SCENEGRAPH_START_LISTENING') ? 
          state.listeners + 1 : state.listeners - 1,
        }
      case 'SCENEGRAPH_UPDATE_PROPERTY':
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
          tag: (action.payload.node.tag === undefined) ? [] : action.payload.node.tag,
          listeners: 0,
        } 
      default:
       return state;
    }
    
}