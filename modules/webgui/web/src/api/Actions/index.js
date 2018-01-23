import { actionTypes } from './actionTypes'

export const updatePropertyValue = ( description, value) => {
	return {
		type: actionTypes.updatePropertyTreeNode,
		payload: {
			URI: description.Identifier, 
			description,
			value,
		}
	};
};

export const changePropertyValue = ( description, value) => {
	return {
		type: actionTypes.changePropertyTreeNode,
		payload: {
			URI: description.Identifier, 
			description,
			value,
		}
	};
};

export const initializePropertyTree = (data) => {
	return {
		type: actionTypes.initializePropertyTree,
		payload: {
			data: data,
		}
	}
}


export const startListening = (URI) => {
	return {
		type: actionTypes.startListeningToNode,
		payload: {
			URI,
		}
	}
}

export const stopListening = (URI) => {
	return {
		type: actionTypes.stopListeningToNode,
		payload: {
			URI,	
		}
	}
}

export const startConnection = () => {
	return {
 		type: actionTypes.startConnection,
		payload: {
				
		}
	}
}

export const onOpenConnection = () => {
	return {
 		type: actionTypes.onOpenConnection,
		payload: {
				
		}
	}
}

export const onCloseConnection = () => {
	return {
 		type: actionTypes.onCloseConnection,
		payload: {

		}
	}
}

export const changeConnectionWait = (value) => {
	return {
 		type: actionTypes.changeConnectionWait,
		payload: {
			value,
		}
	}
}