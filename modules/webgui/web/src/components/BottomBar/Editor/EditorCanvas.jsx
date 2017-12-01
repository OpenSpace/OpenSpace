import React, {Component} from 'react';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import Histogram from './containers/Histogram';
import Envelope from './containers/Envelope'
import { addEnvelope, deleteEnvelope, clearEnvelopes, movePoint } from './actions';
import styles from './EditorCanvas.scss';
import DataManager from '../../../api/DataManager';
import { TransferFunctionKey, HistogramKey } from '../../../api/keys';

class EditorCanvas extends Component {
  constructor(props) {
    super(props);
    this.state = {
      height: 600,
      width: 800,
      color: this.props.color
    }
    this.convertPointsBeforeSending = this.convertPointsBeforeSending.bind(this);
    this.handleRecievedEnvelopes = this.handleRecievedEnvelopes.bind(this);
  }

  componentDidMount() {
    DataManager.getValue(TransferFunctionKey, this.handleRecievedEnvelopes);
  }

  //listen to changes in store
  componentDidUpdate(prevProps, prevState) {
    if (this.props.envelopes !== prevProps.envelopes) {
      this.sendEnvelopes();
    }
  }

  handleRecievedEnvelopes(data) {
    var envelopes = (eval('('+data.Value+')'));
    if (Object.keys(envelopes).length != 0 || envelopes.constructor != Object) {
      this.props.ClearEnvelopes();
      envelopes = envelopes.map(envelope =>
        envelope['points'].map(point =>
            Object.assign({},
                { color : point.color,
                  position : {
                    x : point.position.x * this.state.width,
                    y : this.state.height - point.position.y * this.state.height,
                  },
                })
          )
      )
      envelopes.map(envelope =>
        this.props.AddEnvelope(envelope)
      )
    }
  }

  convertPointsBeforeSending(position) {
      let x = (position.x);
      let y = (this.state.height - position.y);
      return {x: x, y: y};
  }

  sendEnvelopes() {
    let data = this.props.envelopes.map(envelope =>
        Object.assign({},
            {points: envelope.points.map(point =>
                Object.assign({},
                { color : point.color,
                  position : this.convertPointsBeforeSending(point.position),
                })
              )
            },
            {height: this.state.height},
            {width: this.state.width},
          )
      )
    DataManager.setValue(TransferFunctionKey, JSON.stringify(data));
  }

  render() {
    const {height, width, color } = this.state;
    let defaultEnvelopePoints = [{color: color, position : { x: 0, y: height}}, {color: color, position : { x: 30, y: 0}},
                    {color: color, position : { x: 70, y: 0}}, {color: color, position : { x: 100, y: height}}];
      return (
      <div className={styles.EditorContainer} >
        <button onClick={() => this.props.AddEnvelope(defaultEnvelopePoints)}>Add Envelope</button>
        <button onClick={() => this.props.DeleteEnvelope()}>Delete Envelope</button>
        <div className={styles.EnvelopeContainer}>
            {this.props.envelopes.map(envelope =>
              <Envelope className={styles.Envelope}
                key={envelope.id}
                height={height}
                width={width}
                points={envelope.points}
                id={envelope.id}
                active={envelope.active}
              />
            )}
        </div>
        <div >
          <Histogram height={height} width={width}/>
        </div>
      </div>
    );
    }
};
EditorCanvas.propTypes = {
  AddEnvelope: PropTypes.func.isRequired,
  DeleteEnvelope: PropTypes.func.isRequired,
}
const mapStateToProps = (state) => {
  return {
    envelopes:
      state.envelopes,
    };
};

const mapDispatchToProps = (dispatch) => {
  return {
    AddEnvelope: (envelope) => {
      dispatch(addEnvelope(envelope));
    },
    DeleteEnvelope: () => {
      dispatch(deleteEnvelope());
    },
    ClearEnvelopes: () => {
      dispatch(clearEnvelopes());
    },
  }
}

EditorCanvas = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(EditorCanvas)

export default EditorCanvas;
