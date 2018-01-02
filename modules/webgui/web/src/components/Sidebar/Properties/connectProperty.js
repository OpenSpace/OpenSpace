import { connect } from 'react-redux';
import { startListening, stopListening, changePropertyValue } from '../../../api/Actions';

const mapDispatchToProps = (dispatch, ownProps) => {
	  return {
	  	  StartListening: (URI) => {
		    dispatch(startListening(URI))
		  },
		  StopListening: (URI) => {
		    dispatch(stopListening(URI))
		  },
		  ChangeValue: (Value) => {
		    dispatch(changePropertyValue(ownProps.Description, Value))
		  },
	  }
}
export const connectProperty = (Property) => {
    return connect(null, mapDispatchToProps)(Property);
}