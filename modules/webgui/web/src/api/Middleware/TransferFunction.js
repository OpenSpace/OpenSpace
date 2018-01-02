import DataManager from '../DataManager';
import { updatePropertyValue } from '../Actions'
import { SceneGraphKey } from '../keys'

const convertPointsBeforeSending = (position) => {
      let x = (position.x);
      let y = (600 - position.y);
      return {x: x, y: y};
  }

const keepCloning = (objectpassed) => {
    if (objectpassed === null || typeof objectpassed !== 'object') {
       return objectpassed;
    }
    // give temporary-storage the original obj's constructor
    var temporaryStorage = objectpassed.constructor();
    for (var key in objectpassed) {
      temporaryStorage[key] = keepCloning(objectpassed[key]);
    }
    return temporaryStorage;
  }

const convertEnvelopes = (envelopes) => {
    var convertedEnvelopes = keepCloning(envelopes);
    convertedEnvelopes = convertedEnvelopes.map(envelope =>
      Object.assign({},
        {points: envelope.points.map(point =>
            Object.assign({},
            { color : point.color,
              position : convertPointsBeforeSending(point.position),
            })
          )
        },
        {height:600},
        {width: 800},
      )
    )
    return JSON.stringify(convertedEnvelopes);
  }

const startSubscription = (URI, store) => {
	DataManager.subscribe(URI, ({Description, Value}) => {
    handleUpdatedValues(Description, Value, store)
	});	
}

const handleUpdatedValues = (Description, Value, store) => {
  store.dispatch(updatePropertyValue(Description, Value))
  const state = store.getState();
  var property = findUpdatedProperty(state.sceneGraph, Description.Identifier);
  if (property.listeners < 1) {
    DataManager.unsubscribe(Description.Identifier, handleUpdatedValues);
  }
}

const jsonToLua = (json) => {
    return json.replace('[', '').replace(']', '');
}

const sendDataToBackEnd = (key, type, value) => {
	switch(type) {
		case 'Vec2Property':
    case 'Vec3Property':
    case 'Vec4Property':
    case 'MatrixProperty':
    case 'DMat4Property':
			value = jsonToLua(value);
			break;
		case 'TransferFunctionProperty':
			value = convertEnvelopes(value);
			break;
		default:
			break;
	}
	DataManager.setValue(key, value);
}

export const splitURI = (URI) => {
  const indexForName = URI.indexOf('.');
  const name = URI.substring(0, indexForName != -1 ?
    indexForName : URI.length);
  URI = URI.substring(indexForName + 1, URI.length);

  return {name: name, URI: URI, lastOwner: URI.indexOf('.') === -1, lastNode: name === URI}
}

const traverseTree = (node, URI) => {
	const splittedURI = splitURI(URI);
	let tmpValue;
	if (splittedURI.lastNode) {
		node.properties.forEach(function(element) {
			if(element.id === splittedURI.URI) {
				tmpValue = element;
			}
		})
		return tmpValue;
	}
	else {
		node.subowners.forEach(function(element) {
			if( element.name === splittedURI.name) {
				tmpValue = traverseTree(element, splittedURI.URI);
				return tmpValue;
			}
		})
		return tmpValue;
	}
}

const findUpdatedProperty = (state, URI) => {
	let property;
	const splittedURI = splitURI(URI);
	let returnNode;
	state.forEach(function(element) {
		if( element.name === splittedURI.name) {
			returnNode = traverseTree(element, splittedURI.URI);
		}
	})
	return returnNode;
}

const updateBackend = store => next => action => {
  let result = next(action)
  var state = store.getState();
  switch(action.type) {
  	case 'TRANSFERFUNCTION_ADD_TRANSFER_FUNCTION':
  		Object.keys(action.transferfunction.data).forEach(function(key) {
  			if (key !== "TransferFunction")
  				startSubscription(action.transferfunctionId, action.transferfunction.data[key].Description.Identifier, store);
  		})
  		break;
    case 'SCENEGRAPH_CHANGE_PROPERTY':
      var property = findUpdatedProperty(state.sceneGraph, action.payload.URI);
      sendDataToBackEnd(action.payload.URI, property.Description.Type, property.Value)
      break;
    case 'SCENEGRAPH_START_LISTENING':
      var property = findUpdatedProperty(state.sceneGraph, action.payload.URI);
      if(property.listeners === 1)
        console.log("subscribe")
        startSubscription(action.payload.URI, store);
    default:
      break;
  }
  return result
}
export default updateBackend;