import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types'; 

import DataManager from '../../api/DataManager';
import { UploadDataItemScript, ValuePlaceholder } from '../../api/keys';
import { removeLineBreakCharacters, getDirectoryLeaf, getFileBasename } from './utils/helpers';

import Row from '../common/Row/Row';
import Input from '../common/Input/Input/Input';
import styles from './PrepareUploadedData.scss';
import Button from '../common/Input/Button/Button';
import RadioButtons from '../common/Input/RadioButtons/RadioButtons';
import Label from '../common/Label/Label';
import Window from '../common/Window/Window';
import ProgressBar from '../common/ProgressBar/ProgressBar';
import Checkbox from '../common/Input/Checkbox/Checkbox';
import provideWindowWidth from './HOC/provideWindowSize';
import MultiInputs from './presentational/MultiInputs';
import Variables from './presentational/Variables';
import { KEY_DIMENSIONS, KEY_UPPER_DOMAIN_BOUNDS, KEY_LOWER_DOMAIN_BOUNDS } from './constants';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      activated: false,
      volumeProgress: 0,
      uploadButtonIsClicked: false,
      itemName: '',
      gridType: '',

      data: {
        dimensions: { x: 100, y: 100, z: 128 },
        lowerDomainBounds: { r: 1, theta: -90, phi: 0 },
        upperDomainBounds: { r: 15, theta: 90, phi: 360 },
        variable: 'rho',
        rSquared: false,
      }
    };

    this.onChangeMultiInputs = this.onChangeMultiInputs.bind(this);
    this.changeVariable = this.changeVariable.bind(this);
    this.changeRSquared = this.changeRSquared.bind(this);
    this.changeItemName = this.changeItemName.bind(this);
    this.handleGridTypeChange = this.handleGridTypeChange.bind(this);
    this.getDefaultItemName = this.getDefaultItemName.bind(this);
    this.upload = this.upload.bind(this);
    this.handleProgressValue = this.handleProgressValue.bind(this);
    this.subscribeToVolumeConversionProgress = this.subscribeToVolumeConversionProgress.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { selectedFilePaths } = this.props;

    if (selectedFilePaths.length > 0 && JSON.stringify(selectedFilePaths) !== JSON.stringify(prevProps.selectedFilePaths)) {
      this.setState({ 
        activated: true, 
        uploadButtonIsClicked: false
      });
    }

    this.subscribeToVolumeConversionProgress();
  }

  handleProgressValue(data) {
    this.setState({volumeProgress: data.Value});
  }

  subscribeToVolumeConversionProgress() {
    DataManager.subscribe('Modules.DataLoader.Loader.VolumeConversionProgress', this.handleProgressValue);
  }

  onChangeMultiInputs({ currentTarget }, dataToChange) {
    const keyToChange = currentTarget.attributes.label.nodeValue;
    const valueToSet = Number(currentTarget.value);

    this.setState({ 
      data: { 
        ...this.state.data, 
        [dataToChange]: { 
          ...this.state.data[dataToChange], 
          [keyToChange]: valueToSet
        } 
      }
    });
  }
  
  changeVariable(event) {
    this.setState({ data: { ...this.state.data, variable: event.value }});
  }

  changeItemName(event) {
    this.setState({ itemName: event.target.value});
  }

  changeRSquared(checked) {
    this.setState({ data: { ...this.state.data, rSquared: checked }});
  }

  handleGridTypeChange(option) {
    this.setState({ gridType: option });
  }

  getDefaultItemName() {

    // First render base case
    if (this.props.selectedFilePaths[0] === '') {
      const today = new Date();
      return `Unnamed_volume_${today.getFullYear()}_${today.getMonth()}_${today.getDay()}__${today.getHours()}_${today.getMinutes()}`;
    }

    return `${getFileBasename(getDirectoryLeaf(this.props.selectedFilePaths[0]))}_${this.state.data.variable}`
  }

  upload() {
    this.setState({uploadButtonIsClicked: true});

    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, rSquared } = this.state.data;
    let payload = `\'
      return {
        ItemName = "${this.state.itemName || this.getDefaultItemName()}",
        GridType = "${this.state.gridType}",
        Task = {
          Dimensions={${dimensions.x}, ${dimensions.y}, ${dimensions.z}}, 
          Variable="${variable.toLowerCase()}",
          LowerDomainBound={${lowerDomainBounds.r}, ${lowerDomainBounds.theta}, ${lowerDomainBounds.phi}}, 
          UpperDomainBound={${upperDomainBounds.r}, ${upperDomainBounds.theta}, ${upperDomainBounds.phi}}, 
          FactorRSquared="${rSquared.toString()}"
        }
      }
    \'`
    payload = removeLineBreakCharacters(payload);
    const script = UploadDataItemScript.replace(ValuePlaceholder, payload);

    DataManager.runScript(script);
  }

  changeVariable(event) {
    this.setState({ variable: event.value});
  }

  render() {
    const { width, height, currentVolumesConvertedCount, currentVolumesToConvertCount } = this.props;
    const { volumeProgress } = this.state;
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds } = this.state.data;

    const WINDOW_MAX_WIDTH = 400;
    const w = width / 2;
    const h = height / 2;
    const windowSize = {
      width: w > WINDOW_MAX_WIDTH ? WINDOW_MAX_WIDTH : w,
      height: h
    }

    const volumeProgressPercent = Math.floor(volumeProgress * 100);

    if (!this.state.activated) {
      return null;
    }

    return(
      <Window type="small"
              title="Prepare Data"
              size={windowSize}
              position={{ x: 100, y: 200 }}
              closeCallback={() => this.setState({ activated: false })}>
        <div className={styles.content}>
          <div>
          <Input onChange={(event) => this.changeItemName(event)}
                 label='Item name'
                 placeholder='name'
                 value={this.state.itemName || this.getDefaultItemName()} />
          </div>
          <MultiInputs label='Dimensions'
                        options={dimensions} 
                        onChange={(target) => this.onChangeMultiInputs(target, KEY_DIMENSIONS)}/>
          <Variables variable={variable}
                      onChange={this.changeVariable} />
          <MultiInputs label='Lower Domain Bounds'
                        options={lowerDomainBounds} 
                        onChange={(target) => this.onChangeMultiInputs(target, KEY_LOWER_DOMAIN_BOUNDS)}/>
          <MultiInputs label='Upper Domain Bounds'
                        options={upperDomainBounds} 
                        onChange={(target) => this.onChangeMultiInputs(target, KEY_UPPER_DOMAIN_BOUNDS)}/>
          <div><RadioButtons options={['Spherical', 'Cartesian']}
                             defaultOption='Spherical'
                             label='Grid type'
                             onChange={this.handleGridTypeChange} /></div>
          <Checkbox label='Factor r^2?'
                    onChange={this.changeRSquared}/>
          <Button onClick={() => this.upload()}> Convert </Button>
          {this.state.uploadButtonIsClicked && (
            <div>
              <Row>
                <ProgressBar label='Volume conversion progress'
                            initializingMsg='Reading'
                            progressPercent={volumeProgressPercent} />
              </Row>
              <Row className={styles.filesConverted}>
                <Label size="small">{currentVolumesConvertedCount} / {currentVolumesToConvertCount} files complete</Label>
              </Row>
            </div>
          )} 
        </div>
      </Window>
    );
  }
}

PrepareUploadedData.propTypes = {
  selectedFilePaths: PropTypes.arrayOf(PropTypes.string),
  currentVolumesConvertedCount: PropTypes.number,
  currentVolumesToConvertCount: PropTypes.number,
  width: PropTypes.number,
  height: PropTypes.number
};

PrepareUploadedData.defaultProps = {
  selectedFilePaths: [],
  currentVolumesConvertedCount: 0,
  currentVolumesToConvertCount: 0
}

const mapStateToProps = state => ({
  selectedFilePaths: state.dataLoader.selectedFilePaths,
  currentVolumesConvertedCount: state.dataLoader.currentVolumesConvertedCount,
  currentVolumesToConvertCount: state.dataLoader.currentVolumesToConvertCount,
});

PrepareUploadedData = connect(
  mapStateToProps,
  null
)(PrepareUploadedData);

export default provideWindowWidth(PrepareUploadedData);
