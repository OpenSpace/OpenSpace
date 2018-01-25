import DataManager from '../DataManager';
import { updatePropertyValue, initializePropertyTree } from '../Actions'
import * as helperFunctions from '../../utils/propertyTreeHelpers.js'
import { actionTypes } from '../Actions/actionTypes'
import { rootOwnerKey } from '../keys';

let subscriptionIds = []

const handleUpdatedValues = store => ({Description, Value}) => {
  store.dispatch(updatePropertyValue(Description, Value))
  const state = store.getState();
  var property = helperFunctions.findPropertyTreeNode(state.propertyTree, Description.Identifier);
  if (property.listeners < 1) {
    if (DataManager.unsubscribe(Description.Identifier, subscriptionIds[Description.Identifier])) {
      delete subscriptionIds[Description.Identifier];
    }
  }
}


const startSubscription = (URI, store) => {
	subscriptionIds[URI] = DataManager.subscribe(URI, handleUpdatedValues(store));
}

const getPropertyTree = (dispatch) => {
  DataManager.getValue(rootOwnerKey, (Value) => {
    populatePropertyTree(Value, dispatch)
  }); 
}

const populatePropertyTree = (Value, dispatch) => {
  dispatch(initializePropertyTree(Value.subowners));
}

const sendDataToBackEnd = (node) => {
	switch(node.Description.Type) {
		case 'Vec2Property':
    case 'Vec3Property':
    case 'Vec4Property':
    case 'MatrixProperty':
    case 'DMat4Property':
			const convertedValue = helperFunctions.jsonToLuaTable(node.Value);
      DataManager.setValue(node.Description.Identifier, convertedValue);
			break;
		case 'TransferFunctionProperty':
			const convertedEnvelopes = helperFunctions.convertEnvelopes(node.Value);
      DataManager.setValue(node.Description.Identifier, convertedEnvelopes);
			break;
    case 'TriggerProperty':
      DataManager.trigger(node.Description.Identifier);
      break;
    case 'StringProperty':
      const convertedString = helperFunctions.jsonToLuaString(node.Value);
      DataManager.setValue(node.Description.Identifier, convertedString);
      break;
		default:
      DataManager.setValue(node.Description.Identifier, node.Value);
			break;
	}
}

export const updateBackend = store => next => action => {
  let result = next(action);
  var state = store.getState();
  switch(action.type) {
    case actionTypes.onOpenConnection:
      getPropertyTree(store.dispatch);
      break;
    case actionTypes.changePropertyTreeNode:
      var node = helperFunctions.findPropertyTreeNode(state.propertyTree, action.payload.URI);
      sendDataToBackEnd(node)
      break;
    case actionTypes.startListeningToNode:
      var node = helperFunctions.findPropertyTreeNode(state.propertyTree, action.payload.URI);
      if(node.listeners === 1)
        startSubscription(action.payload.URI, store);
    default:
      break;
  }
  return result
}
export default updateBackend;