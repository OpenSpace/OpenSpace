import React, {Component} from 'react';
import PropTypes from 'prop-types';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import { movePoint, toggleActiveEnvelope} from './actions';
import Envelope from './Envelope';
import styles from './EnvelopeCanvas.scss';

class EnvelopeCanvas extends Component{
  constructor(props){
    super(props);
  }

  render() {
  const { envelopes, onPointDrag, height, width } = this.props;
    return(
    <div  className={styles.EnvelopeCanvas}>
      {envelopes.map(envelope =>
        <Envelope className={styles.Envelope}
          key={envelope.id}
          {...envelope}
          height={height}
          width={width}
          envelope={envelope}
          handleOnDrag={(position, id, envelopeId) => onPointDrag(position, id, envelopeId)}
        />
      )}
    </div>
    )
  }
}
EnvelopeCanvas.propTypes = {
  onPointDrag: PropTypes.func.isRequired,
  envelopes: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      points: PropTypes.arrayOf(
        PropTypes.shape({
            id: PropTypes.number.isRequired,
            position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
          }).isRequired,
      ).isRequired,
    }).isRequired
  ).isRequired,
}

const mapStateToProps = (state) => {
  return {
    envelopes:
      state.envelopes,
    };
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    onPointDrag: (position, id, envelopeId) => {
      dispatch(movePoint(id, envelopeId, position));
      ownProps.onPointMoved();
    },
  }
}

const CreatedEnvelopes = connect(
  mapStateToProps,
  mapDispatchToProps,
)(EnvelopeCanvas);

export default CreatedEnvelopes;
