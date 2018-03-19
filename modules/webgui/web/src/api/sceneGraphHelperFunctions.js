export const splitURI = (URI) => {
  const indexForIdentifier = URI.indexOf('.');
  const identifier = URI.substring(0, indexForIdentifier != -1 ?
    indexForIdentifier : URI.length);
  URI = URI.substring(indexForIdentifier + 1, URI.length);

  return { identifier, URI, isLastOwner: URI.indexOf('.') === -1, isLastNode: URI.length === 0 };
};

export const getIdOfProperty = (URI) => {
  const indexForIdentifier = URI.lastIndexOf('.');
  return URI.substring(indexForIdentifier != -1 ?
    indexForIdentifier + 1 : 0, URI.length);
};

export const changePropertyOwnerState = (state, action) => {
  switch (action.type) {
    case 'SCENEGRAPH_START_LISTENING':
    case 'SCENEGRAPH_START_LISTENING':
      return {
        ...state,
        listeners: (action.type == 'SCENEGRAPH_START_LISTENING') ?
          state.listeners + 1 : state.listeners - 1,
      };
    case 'SCENEGRAPH_UPDATE_PROPERTY':
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

const convertPointsBeforeSending = (position) => {
  const x = (position.x);
  const y = (600 - position.y);
  return { x, y };
};

const keepCloning = (objectpassed) => {
  if (objectpassed === null || typeof objectpassed !== 'object') {
    return objectpassed;
  }
  // give temporary-storage the original obj's constructor
  const temporaryStorage = objectpassed.constructor();
  for (const key in objectpassed) {
    temporaryStorage[key] = keepCloning(objectpassed[key]);
  }
  return temporaryStorage;
};

const convertEnvelopes = (envelopes) => {
  let convertedEnvelopes = keepCloning(envelopes);
  convertedEnvelopes = convertedEnvelopes.map(envelope =>
    Object.assign({},
      { points: envelope.points.map(point =>
        Object.assign({},
          { color: point.color,
            position: point.position,
          }),
      ),
      },
    ),
  );
  return JSON.stringify(convertedEnvelopes);
};


const jsonToLua = json => json.replace('[', '').replace(']', '');

const traverseTree = (node, URI) => {
  const splittedURI = splitURI(URI);
  let tmpValue;
  if (splittedURI.isLastNode) {
    node.properties.forEach((element) => {
      if (element.id === splittedURI.URI) {
        tmpValue = element;
      }
    });
    return tmpValue;
  }

  node.subowners.forEach((element) => {
    if (element.identifier === splittedURI.identifier) {
      tmpValue = traverseTree(element, splittedURI.URI);
      return tmpValue;
    }
  });
  return tmpValue;
};

const findUpdatedProperty = (state, URI) => {
  let property;
  const splittedURI = splitURI(URI);
  let returnNode;
  state.forEach((element) => {
    if (element.identifier === splittedURI.identifier) {
      returnNode = traverseTree(element, splittedURI.URI);
    }
  });
  return returnNode;
};
