import React, {Component} from 'react';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { addEnvelope, deleteEnvelope, clearEnvelopes, addPoint, changeColor } from '../actions';
import EditorContainer from '../presentational/EditorContainer'
import styles from '../style/EditorCanvas.scss';
import DataManager from '../../../../api/DataManager';
import { TransferFunctionKey } from '../../../../api/keys';

class Editor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      activeVolume: this.props.volumes[0],
      height: 600,
      width: 800,
    }
    this.handleVolumeChange = this.handleVolumeChange.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.props.color !== prevProps.color) {
      this.props.ChangeColor(this.props.color, this.state.activeVolume);
    }
  }

  handleVolumeChange(event) {
    this.setState({activeVolume: event.target.value});
  }

  render() {
    const {height, width, activeVolume } = this.state;
    const { color } = this.props;
    let defaultEnvelopePoints = [{color: color, position : { x: 0, y: height}}, {color: color, position : { x: 30, y: 0}},
                    {color: color, position : { x: 70, y: 0}}, {color: color, position : { x: 100, y: height}}];
    return (
      <div>
        <button onClick={() => this.props.AddEnvelope(defaultEnvelopePoints, activeVolume)}>Add Envelope</button>
        <button onClick={() => this.props.DeleteEnvelope(activeVolume)}>Delete Envelope</button>
        <button onClick={() => this.props.AddPoint(activeVolume)}>Add Point</button>
        <select onChange={this.handleVolumeChange}>{
          this.props.volumes.map((volume, index) =>
            <option key={index} value={volume}>{volume}</option>
            )
        }
        </select>
        <EditorContainer
          height={height}
          width={width}
          activeVolume={this.state.activeVolume}
        />
      </div>
    );
  }
};
Editor.propTypes = {
  AddEnvelope: PropTypes.func.isRequired,
  AddPoint: PropTypes.func.isRequired,
  DeleteEnvelope: PropTypes.func.isRequired,
  ClearEnvelopes: PropTypes.func.isRequired,
}

const mapStateToProps = (state) => {
  let volumes = state.transferfunctions.map(transferfunction => {
      return transferfunction.id;
    })
    return {
      volumes
    }
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    AddEnvelope: (envelope, activeVolume) => {
      dispatch(addEnvelope(envelope, activeVolume));
    },
    AddPoint: (color, activeVolume) => {
      dispatch(addPoint(color, activeVolume));
    },
    DeleteEnvelope: (activeVolume) => {
      dispatch(deleteEnvelope(activeVolume));
    },
    ClearEnvelopes: (activeVolume) => {
      dispatch(clearEnvelopes(activeVolume));
    },
    ChangeColor: (color, activeVolume) => {
      dispatch(changeColor(color, activeVolume));
    },
  }
}

Editor = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(Editor)

export default Editor;
