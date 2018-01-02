import React, {Component} from 'react';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { addEnvelope, deleteEnvelope, clearEnvelopes, addPoint, changeColor } from '../../../../api/Actions/transferFunctionActions.js';
import EditorContainer from '../presentational/EditorContainer'

class Editor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      URI: this.props.volumes[0],
      height: 600,
      width: 800,
    }
    this.handleVolumeChange = this.handleVolumeChange.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.props.color !== prevProps.color) {
      this.props.ChangeColor(this.props.color, this.props.URI);
    }
  }

  handleVolumeChange(event) {
    this.setState({URI: event.target.value});
  }

  render() {
    const {height, width } = this.state;
    const { color, URI, volumes } = this.props;
    let defaultEnvelopePoints = [{color: color, position : { x: 0, y: height}}, {color: color, position : { x: 30, y: 0}},
                    {color: color, position : { x: 70, y: 0}}, {color: color, position : { x: 100, y: height}}];
    return (
      <div>
        <button onClick={() => this.props.AddEnvelope(defaultEnvelopePoints, URI)}>Add Envelope</button>
        <button onClick={() => this.props.DeleteEnvelope(URI)}>Delete Envelope</button>
        <button onClick={() => this.props.AddPoint(color, URI)}>Add Point</button>
        <select onChange={this.handleVolumeChange}>{
          this.props.volumes.map((volume, index) =>
            <option key={index} value={volume}>{volume}</option>
            )
        }
        </select>
        <EditorContainer
          height={height}
          width={width}
          URI={this.state.URI}
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
  let volumes = state.sceneGraph.map(node => {
      if(node.name === "Enlil Sequence")
        return node.id;
      })
    return {
      volumes,
      URI: "Enlil Sequence.renderable.TransferFunctionHandler.TransferFunction"
    }
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    AddEnvelope: (envelope, URI) => {
      dispatch(addEnvelope(envelope, URI));
    },
    AddPoint: (color, URI) => {
      dispatch(addPoint(color, URI));
    },
    DeleteEnvelope: (URI) => {
      dispatch(deleteEnvelope(URI));
    },
    ClearEnvelopes: (URI) => {
      dispatch(clearEnvelopes(URI));
    },
    ChangeColor: (color, URI) => {
      dispatch(changeColor(color, URI));
    },
  }
}

Editor = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(Editor)

export default Editor;
