export const updatePropertyValue = ( description, value) => {
	return {
		type: 'SCENEGRAPH_UPDATE_PROPERTY',
		payload: {
			URI: description.Identifier, 
			description,
			value,
		}
	};
};

export const changePropertyValue = ( description, value) => {
	console.log(description)
	return {
		type: 'SCENEGRAPH_CHANGE_PROPERTY',
		payload: {
			URI: description.Identifier, 
			description,
			value,
		}
	};
};

export const insertInSceneGraph = (data) => {
	console.log(data)
	return {
		type: 'SCENEGRAPH_INSERT',
		payload: {
			data: data,
		}
	}
}


export const startListening = (URI) => {
	return {
		type: 'SCENEGRAPH_START_LISTENING',
		payload: {
			URI,
		}
	}
}

export const stopListening = (URI) => {
	return {
		type: 'SCENEGRAPH_STOP_LISTENING',
		payload: {
			URI,	
		}
	}
}