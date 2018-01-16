import DataManager from '../DataManager';
import { updatePropertyValue } from '../Actions'
import { SceneGraphKey } from '../keys'
import * as helperFunctions from '../sceneGraphHelperFunctions.js'

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