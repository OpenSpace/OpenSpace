import React from 'react';
import PropTypes from 'prop-types';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import { movePoint, toggleActiveEnvelope} from './actions';
import Envelope from './Envelope';
import styles from './EnvelopeCanvas.scss';

const EnvelopeCanvas =  ({
  envelopes,
  onPointDrag,
  onPointClick,
}) => (
    <div  className={styles.EnvelopeCanvas}>
      {envelopes.map(envelope =>
        <Envelope className={styles.Envelope}
          key={envelope.id}
          {...envelope}
          handleOnDrag={(position, id) => onPointDrag(position, id, envelope.id)}
          handleOnClick={() => onPointClick(envelope.id)}
        />
      )}
    </div>
);
EnvelopeCanvas.propTypes = {
  onPointDrag: PropTypes.func.isRequired,
  onPointClick: PropTypes.func.isRequired,
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
      color: PropTypes.string.isRequired,
      active: PropTypes.bool.isRequired,
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
    },
    onPointClick: (id) => {
      dispatch(toggleActiveEnvelope(id));
    }
  }
}

const CreatedEnvelopes = connect(
  mapStateToProps,
  mapDispatchToProps,
)(EnvelopeCanvas);

export default CreatedEnvelopes;
