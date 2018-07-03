import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types'; 

import DataManager from '../../api/DataManager';
import { UploadDataItemScript, ValuePlaceholder } from '../../api/keys';
import { removeLineBreakCharacters, getDirectoryLeaf } from './utils/helpers';

import Row from '../common/Row/Row';
import styles from './PrepareUploadedData.scss';
import CenteredLabel from '../common/CenteredLabel/CenteredLabel';
import Button from '../common/Input/Button/Button';
import Window from '../common/Window/Window';
import ProgressBar from '../common/ProgressBar/ProgressBar';
import Checkbox from '../common/Input/Checkbox/Checkbox';
import provideWindowWidth from './HOC/provideWindowSize';
import OptionSelect from './presentational/OptionSelect';
import Variables from './presentational/Variables';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      volumeProgress: 0,
      uploadButtonClicked: false,

      activated: false,
      dimensions: { x: 100, y: 100, z: 128 },
      lowerDomainBounds: { r: 1, theta: -90, phi: 0 },
      upperDomainBounds: { r: 15, theta: 90, phi: 360 },
      variable: 'rho',
      rSquared: false,
    };

    this.changeDimensions = this.changeDimensions.bind(this);
    this.changeLowerDomainBounds = this.changeLowerDomainBounds.bind(this);
    this.changeUpperDomainBounds = this.changeUpperDomainBounds.bind(this);
    this.changeVariable = this.changeVariable.bind(this);
    this.changeRSquared = this.changeRSquared.bind(this);
    this.upload = this.upload.bind(this);
    this.handleProgressValue = this.handleProgressValue.bind(this);
    this.subscribeToVolumeConversionProgress = this.subscribeToVolumeConversionProgress.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { filePaths } = this.props;

    if( filePaths !== prevProps.filePaths && filePaths !== undefined ) {
      this.setState({ activated: true });
    }

    this.subscribeToVolumeConversionProgress();
  }

  handleProgressValue(data) {
    this.setState({volumeProgress: data.Value});
  }

  subscribeToVolumeConversionProgress() {
    DataManager.subscribe('Modules.DataLoader.Loader.VolumeConversionProgress', this.handleProgressValue);
  }

  // TODO: Generalize the onChange function of OptionSelect!
  // Gets the corresponding key of the last changed value in dimensions.
  // Assigns the changed value to the correct key of dimensions.
  changeDimensions({ currentTarget }) {
    let tempDim = this.state.dimensions;
    let key = currentTarget.attributes.label.nodeValue;
    tempDim[key] = Number(currentTarget.value);

    this.setState({ dimensions: tempDim });
  }

  changeLowerDomainBounds({ currentTarget }) {
    let tempBound = this.state.lowerDomainBounds;
    let key = currentTarget.attributes.label.nodeValue;
    tempBound[key] = Number(currentTarget.value);

    this.setState({ lowerDomainBounds: tempBound });
  }
  
  changeUpperDomainBounds({ currentTarget }) {
    let tempBound = this.state.upperDomainBounds;
    let key = currentTarget.attributes.label.nodeValue;
    tempBound[key] = Number(currentTarget.value);

    this.setState({ upperDomainBounds: tempBound });
  }

  changeVariable(event) {
    this.setState({ variable: event.value });
  }

  changeRSquared(checked) {
    this.setState({ rSquared: checked });
  }

  upload() {
    this.setState({uploadButtonClicked: true});

    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, rSquared } = this.state;
    let data = `\'
      return {
        Input="${this.props.filePaths}",
        Dimensions={${dimensions.x}, ${dimensions.y}, ${dimensions.z}}, 
        Variable="${variable.toLowerCase()}",
        LowerDomainBound={${lowerDomainBounds.r}, ${lowerDomainBounds.theta}, ${lowerDomainBounds.phi}}, 
        UpperDomainBound={${upperDomainBounds.r}, ${upperDomainBounds.theta}, ${upperDomainBounds.phi}}, 
        FactorRSquared="${rSquared.toString()}"
      }
    \'`
    data = removeLineBreakCharacters(data);
    const script = UploadDataItemScript.replace(ValuePlaceholder, data);

    DataManager.runScript(script);
  }

  changeVariable(event) {
    this.setState({ variable: event.value});
  }

  render() {
    const { width, height } = this.props;
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, volumeProgress } = this.state;

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
<<<<<<< HEAD
              position={{ x: 300, y: 300 }}
=======
              position={{ x: 100, y: 200 }}
>>>>>>> Run file dialog on separate thread and connect chosen file to GUI
              closeCallback={() => this.setState({ activated: false })}>
        <div className={styles.content}>
          <CenteredLabel>{getDirectoryLeaf(this.props.filePaths)}</CenteredLabel>
          <OptionSelect label='Dimensions'
                        options={dimensions} 
                        onChange={this.changeDimensions}/>
          <Variables variable={variable}
                      onChange={this.changeVariable} />
          <OptionSelect label='Lower Domain Bounds'
                        options={lowerDomainBounds} 
                        onChange={this.changeLowerDomainBounds}/>
          <OptionSelect label='Upper Domain Bounds'
                        options={upperDomainBounds} 
                        onChange={this.changeUpperDomainBounds}/>
          <Checkbox label='Factor r^2?'
                    onChange={this.changeRSquared}/>
          <Button onClick={() => this.upload()}> Convert </Button>
          {this.state.uploadButtonClicked && (
            <Row>
              <ProgressBar label='Volume conversion progress'
                           initializingMsg='Reading'
                           progressPercent={volumeProgressPercent} />
            </Row>
          )} 
        </div>
      </Window>
    );
  }
}

PrepareUploadedData.propTypes = {
  filePaths: PropTypes.string,
  width: PropTypes.number,
  height: PropTypes.number
};

PrepareUploadedData.defaultProps = {
  filePaths: '',
}

const mapStateToProps = state => ({
  filePaths: state.dataLoader.filePaths
});

PrepareUploadedData = connect(
  mapStateToProps,
  null
)(PrepareUploadedData);

export default provideWindowWidth(PrepareUploadedData);