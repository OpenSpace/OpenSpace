import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import TfIcon from 'svg-react-loader?name=TfIcon!../images/tf.svg';
import { addEnvelope, deleteEnvelope, clearEnvelopes, addPoint, changeColor } from '../../../../api/Actions/transferFunctionActions';
import EditorContainer from '../presentational/EditorContainer';
import { startListening, stopListening } from '../../../../api/Actions';
import { findAllNodesWithTag } from '../../../../utils/propertyTreeHelpers';
import Window from '../../../common/Window/Window';
import Picker from '../../Picker';
import styles from '../style/TfEditor.scss';
import ColorPicker from './ColorPicker';
import Button from '../../../common/Input/Button/Button';
import Select from '../../../common/Input/Select/Select';
import SmallLabel from '../../../common/SmallLabel/SmallLabel';

class TfEditor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentVolume: null,
      URI: null,
      color: '#ffffff',
      editorSize: {
        height: 600,
        width: 800,
      },
      showTfEditor: false,
    };
    this.handleVolumeChange = this.handleVolumeChange.bind(this);
    this.toggleTfEditor = this.toggleTfEditor.bind(this);
  }

  componentDidMount() {
    this.props.volumes.forEach((volume) => {
      volume.data.properties.forEach((property) => {
        this.props.StartListening(property.Description.Identifier);
      });
    });
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.currentVolume === null && this.props.volumes.length !== 0) {
      this.setState({ currentVolume: this.props.volumes[0].name });
      const URI = this.props.volumes[0].data.properties.find(obj => obj.id === 'TransferFunction').Description.Identifier;
      this.setState({ URI });
    }
    if (this.state.color !== prevState.color) {
      const { currentVolume } = this.state;
      this.props.ChangeColor(this.state.color, this.props.volumes.find(obj => obj.id === currentVolume.name).data.properties.find(obj => obj.id === 'TransferFunction').Description.Identifier);
    }
  }

  componentWillUnmount() {
    this.props.volumes.forEach((volume) => {
      volume.data.properties.forEach((property) => {
        this.props.StopListening(property.Description.Identifier);
      });
    });
  }

  toggleTfEditor() {
    this.setState({ showTfEditor: !this.state.showTfEditor });
  }

  handleVolumeChange(event) {
    this.setState({ currentVolume: event.value });
    const URI = this.props.volumes.find(obj => obj.id === this.state.currentVolume.name).data.properties.find(obj => obj.id === 'TransferFunction').Description.Identifier;
    this.setState({ URI });
  }

  render() {
    const { color, editorSize, currentVolume, showTfEditor, URI } = this.state;
    const { volumes } = this.props;

    const defaultEnvelopePoints = [
      { color, position: { x: 0, y: 0 } },
      { color, position: { x: 0.3, y: 1 } },
      { color, position: { x: 0.7, y: 1 } },
      { color, position: { x: 1, y: 0 } }];

    return (
      <div >
        { (volumes.length !== 0) && (
          <div className={styles.Wrapper}>
            <Picker onClick={this.toggleTfEditor} className={(showTfEditor ? styles.Active : '')}>
              <div className={styles.FlexColumn}>
                <TfIcon className={styles.iconImage} />
                <SmallLabel>Tf-Editor</SmallLabel>
              </div>
            </Picker>
            <div>
              { showTfEditor && (
                <div>
                  <Window size={{ width: 1200, height: 700 }} closeCallback={this.toggleTfEditor} title={'Transfer Function Editor'} className={styles.Window}>
                    <div className={styles.Canvas}>
                      <div className={styles.FlexContainer}>
                        <EditorContainer
                          height={editorSize.height}
                          width={editorSize.width}
                          activeVolume={volumes.find(obj => obj.id === currentVolume.name).data}
                          URI={URI}
                        />
                        <div className={styles.FlexColumn}>
                          <ColorPicker
                            className={styles.ColorPicker}
                            onColorChange={color => this.setState({ color })}
                          />
                          <div className={styles.FlexColumn}>
                            <Button className={styles.Button} onClick={() => this.props.AddEnvelope(defaultEnvelopePoints, URI)} children={' Add Envelope'} />
                            <Button className={styles.Button} onClick={() => this.props.DeleteEnvelope(URI)} children={' Delete Envelope'} />
                            <Button className={styles.Button} onClick={() => this.props.AddPoint(color, URI)} children={' Add Point'} />

                            <Select
                              onChange={this.handleVolumeChange}
                              label={'Select Volume'}
                              value={this.state.currentVolume}
                              options={this.props.volumes
                                .map(volume => ({ value: volume.name, label: volume.name }))}
                            />
                          </div>
                        </div>
                      </div>
                    </div>
                  </Window>
                </div>
              )}
            </div>
          </div>
        )}
      </div>
    );
  }
}
TfEditor.propTypes = {
  AddEnvelope: PropTypes.func.isRequired,
  AddPoint: PropTypes.func.isRequired,
  DeleteEnvelope: PropTypes.func.isRequired,
  ClearEnvelopes: PropTypes.func.isRequired,
};

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  const TfTag = 'TF';
  let volumes = [];
  if (Object.keys(state.propertyTree).length !== 0) {
    let nodes = [];
    const rootNodes = state.propertyTree.subowners.filter(element => element.name === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    volumes = findAllNodesWithTag(nodes, TfTag);
  }
  return {
    volumes,
  };
};

const mapDispatchToProps = dispatch => ({
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
    dispatch(startListening(URI));
  },
  StopListening: (URI) => {
    dispatch(stopListening(URI));
  },
});

TfEditor = connect(
  mapStateToProps,
  mapDispatchToProps,
)(TfEditor);

export default TfEditor;
