import React, {Component} from 'react';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import Histogram from './Histogram';
import CreatedEnvelopes from './EnvelopeCanvas'
import { addEnvelope, deleteEnvelope, clearEnvelopes } from './actions';
import styles from './EditorCanvas.scss';
import DataManager from '../../../api/DataManager';
import { TransferFunctionKey, HistogramKey } from '../../../api/keys';

class EditorCanvas extends Component {
  constructor(props) {
    super(props);
    this.state = {
      height: 600,
      width: 800,
      hasHistogramData: false,
      color: this.props.color,
    }

    this.sendEnvelopes = this.sendEnvelopes.bind(this);
    this.convertPointsBeforeSending = this.convertPointsBeforeSending.bind(this);
    this.handleRecievedHistogram = this.handleRecievedHistogram.bind(this);
    this.handleRecievedEnvelopes = this.handleRecievedEnvelopes.bind(this);
  }

  componentDidMount() {
    DataManager.getValue(TransferFunctionKey, this.handleRecievedEnvelopes);
    DataManager.getValue(HistogramKey, this.handleRecievedHistogram);
    DataManager.subscribe(HistogramKey, this.handleRecievedHistogram);
  }

  handleRecievedHistogram(data) {
    var convertedData = (eval('('+data.Value+')'));
    return this.normalizeHistogramDataToCanvas(convertedData);
  }

  handleRecievedEnvelopes(data) {
    var envelopes = (eval('('+data.Value+')'));
    console.log(envelopes);
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

  normalizeHistogramDataToCanvas(data) {
    if(data.length < this.state.width) {
      let maxValue =  Math.max.apply(Math,data.map(function(o){return o;}));
      this.state.NormalizedHistogramData = data.map((value, index) =>
          Object.assign({},
              {x: (index / data.length) * this.state.width,
               y: (value / maxValue) * this.state.height,
              },
        )
      )
      //Making sure the graphs body gets filled properly by adding 0 in the beginning and end of the histogram
      this.state.NormalizedHistogramData.unshift({ x: 0, y: 0 });
      this.state.NormalizedHistogramData.push({ x: this.state.width, y: 0 });
      this.setState({ histogramLoaded : true})
    }
  }

  render() {
    const {histogramLoaded, NormalizedHistogramData, height, width, color } = this.state;
    let defaultEnvelopePoints = [{color: color, position : { x: 0, y: height}}, {color: color, position : { x: 30, y: 0}},
                    {color: color, position : { x: 70, y: 0}}, {color: color, position : { x: 100, y: height}}];
      return (
      <div className={styles.EditorContainer} >
        <button onClick={() => this.props.AddEnvelope(defaultEnvelopePoints)}>Add Envelope</button>
        <button onClick={() => this.props.DeleteEnvelope()}>Delete Envelope</button>
        <div className={styles.EnvelopeContainer}>
            <CreatedEnvelopes onPointMoved={() => this.sendEnvelopes()} height={height} width={width}/>
        </div>
        {histogramLoaded && (
        <div className={styles.HistogramContainer}>
          <Histogram data={NormalizedHistogramData} height={height} width={width}/>
        </div>
      )}
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

const mapDispatchToProps = (dispatch, ownProps) => {
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
