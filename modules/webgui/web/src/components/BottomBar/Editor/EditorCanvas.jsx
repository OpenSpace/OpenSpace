import React, {Component} from 'react';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import CreatedHistograms from './Histogram';
import CreatedEnvelopes from './EnvelopeCanvas'
import { addEnvelope, addHistogram, deleteEnvelope } from './actions';
import styles from './EditorCanvas.scss';
import DataManager from '../../../api/DataManager';
import { TransferFunctionKey } from '../../../api/keys';

class EditorCanvas extends Component {
  constructor(props) {
    super(props);
    this.handleChange = this.handleChange.bind(this);
    this.convertPointsBeforeSending = this.convertPointsBeforeSending.bind(this);
  }

  componentDidMount() {
    var histPositions = [{x: 0, y: 0}, {x: 300, y: 500},{x: 400, y: 600}, {x: 800, y: 0}];
    this.props.AddHistogram(histPositions);
  }

  convertPointsBeforeSending(position) {
      let x = (position.x - 10) / 800;
      let y = (600 - position.y) / 590;
      return {x: x, y: y};
  }

  handleChange(envelopes) {
      let data = envelopes.map(envelope =>
          Object.assign({},
              {color: envelope.color},
              {points: envelope.points.map(point =>
                  Object.assign({},
                  {position : this.convertPointsBeforeSending(point.position)}
                  )
                )
              },
            )
        )
      console.log(data);
      DataManager.setValue(TransferFunctionKey, JSON.stringify(data));
  }

  render() {
    var positions = [{x: 10, y: 600}, {x: 30, y: 50},{x: 40, y: 60}, {x: 50, y: 600}];
      return (
      <div className={styles.EditorContainer} >
        <button onClick={() => this.props.AddEnvelope(positions)}>Add Envelope</button>
        <button onClick={() => this.handleChange(this.props.envelopes)}>Send Envelopes</button>
        <button onClick={() => this.props.DeleteEnvelope()}>Delete Envelope</button>
        <div className={styles.EnvelopeContainer}>
          <CreatedEnvelopes />
        </div>
        <div className={styles.HistogramContainer}>
          <CreatedHistograms />
        </div>
      </div>
    );
    }
};
EditorCanvas.propTypes = {
  AddEnvelope: PropTypes.func.isRequired,
  AddHistogram: PropTypes.func.isRequired,
  DeleteEnvelope: PropTypes.func.isRequired,
}
const mapStateToProps = (state) => {
  return {
    envelopes:
      state.envelopes,
    };
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    AddEnvelope: (data) => {
      dispatch(addEnvelope(data, 'red'));
    },
    AddHistogram: (data) => {
      dispatch(addHistogram(data, 'blue'));
    },
    DeleteEnvelope: () => {
      dispatch(deleteEnvelope());
    },
  }
}

EditorCanvas = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(EditorCanvas)

export default EditorCanvas;
