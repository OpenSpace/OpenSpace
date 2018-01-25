import React, {Component} from 'react';
import Draggable from 'react-draggable';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { addEnvelope, deleteEnvelope, clearEnvelopes, addPoint, changeColor } from '../../../../api/Actions/transferFunctionActions.js';
import EditorContainer from '../presentational/EditorContainer'
import { startListening, stopListening } from '../../../../api/Actions';

class Editor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentVolume: this.props.volumes[0].name,
      height: 600,
      width: 800,
    }
    this.handleVolumeChange = this.handleVolumeChange.bind(this);
  }

  componentDidMount() {
    this.props.volumes.forEach(volume => {
      volume.TransferFunctionData.properties.forEach(property => {
        this.props.StartListening(property.Description.Identifier);
      })
    })
  }

  componentWillUnmount() {
    this.props.volumes.forEach(volume => {
      volume.TransferFunctionData.properties.forEach(property => {
        this.props.StopListening(property.Description.Identifier);
      })
    })
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.props.color !== prevProps.color) {
      const { currentVolume } = this.state;
      this.props.ChangeColor(this.props.color, this.props.volumes.find(function (obj) 
        { return obj.id === currentVolume.name; }).TransferFunctionData.properties.find(function (obj) 
        { return obj.id === "TransferFunction"; }).Description.Identifier);
    }
  }

  handleVolumeChange(event) {
    this.setState({currentVolume: event.target.value});
  }

  render() {
    const { height, width, currentVolume } = this.state;
    const { color, volumes } = this.props;

    const URI = volumes.find(function (obj) { 
      return obj.id === currentVolume.name; }).TransferFunctionData.properties.find(function (obj) { 
        return obj.id === "TransferFunction"; }).Description.Identifier;
    
    const defaultEnvelopePoints = [{color: color, position : { x: 0, y: 0}}, {color: color, position : { x: 0.3, y: 1}},
                    {color: color, position : { x: 0.7, y: 1}}, {color: color, position : { x: 1, y: 0}}];
    return (
      <div>
        <button onClick={() => this.props.AddEnvelope(defaultEnvelopePoints, URI)}>Add Envelope</button>
        <button onClick={() => this.props.DeleteEnvelope(URI)}>Delete Envelope</button>
        <button onClick={() => this.props.AddPoint(color, URI)}>Add Point</button>
        
        <select onChange={this.handleVolumeChange}>{
          this.props.volumes.map((volume, index) =>
            <option key={index} value={volume.name}>{volume.name}</option>
            )
        }
        </select>

        {(currentVolume !== undefined) && (
        <EditorContainer
          height={height}
          width={width}
          activeVolume={volumes.find(function (obj) { return obj.id === currentVolume.name; }).TransferFunctionData}
          URI={URI}
        />
        )}
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

const traverseTree = (node) => {
  let TransferFunctionData;
  node.subowners.forEach(function(element) {
        TransferFunctionData = traverseTree(element);
        if( element.tag.includes("TF")) {
          TransferFunctionData = element;
        }
    })

  return TransferFunctionData;
}

const findAllVolumes = (state) => {
  let volumes = [];
  state.forEach(function(element) {
      const TransferFunctionData = traverseTree(element);
      if(TransferFunctionData !== undefined) {
        const returnValue = {
          TransferFunctionData: TransferFunctionData,
          name: element.name
        }
        volumes.push(returnValue);
      }
  })
  return volumes;
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
    const rootNodes = state.propertyTree.filter(element => element.name == sceneType)
    let nodes = [];
    rootNodes.forEach(function(node) {
      nodes = [...nodes, ...node.subowners]; 
    })
  let volumes = findAllVolumes(nodes);
  return {
    volumes,
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
    StartListening: (URI) => {
      dispatch(startListening(URI))
    },
    StopListening: (URI) => {
      dispatch(stopListening(URI))
    },
  }
}

Editor = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(Editor)

export default Editor;
