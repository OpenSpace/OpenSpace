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
import { KEY_DIMENSIONS, KEY_UPPER_DOMAIN_BOUNDS, KEY_LOWER_DOMAIN_BOUNDS } from './constants';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      activated: false,
      volumeProgress: 0,
      uploadButtonIsClicked: false,

      data: {
        dimensions: { x: 100, y: 100, z: 128 },
        lowerDomainBounds: { r: 1, theta: -90, phi: 0 },
        upperDomainBounds: { r: 15, theta: 90, phi: 360 },
        variable: 'rho',
        rSquared: false,
      }
    };

    this.onChangeOptionSelect = this.onChangeOptionSelect.bind(this);
    this.changeVariable = this.changeVariable.bind(this);
    this.changeRSquared = this.changeRSquared.bind(this);
    this.upload = this.upload.bind(this);
    this.handleProgressValue = this.handleProgressValue.bind(this);
    this.subscribeToVolumeConversionProgress = this.subscribeToVolumeConversionProgress.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { filePaths } = this.props;

    if( filePaths !== prevProps.filePaths && filePaths !== undefined ) {
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

  onChangeOptionSelect({ currentTarget }, type) {
    const keyToChange = currentTarget.attributes.label.nodeValue;
    const valueToSet = Number(currentTarget.value);
    let dataToChange = ''

    switch (type) {
      case KEY_DIMENSIONS:
        dataToChange = 'dimensions'
        break;
      case KEY_UPPER_DOMAIN_BOUNDS:
        dataToChange = 'upperDomainBounds'
        break;
      case KEY_LOWER_DOMAIN_BOUNDS:
        dataToChange = 'lowerDomainBounds'
        break;
      default:
        return;
    }

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

  changeRSquared(checked) {
    this.setState({ data: { ...this.state.data, rSquared: checked }});
  }

  upload() {
    this.setState({uploadButtonIsClicked: true});

    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, rSquared } = this.state.data;
    let payload = `\'
      return {
        Input="${this.props.filePaths}",
        Dimensions={${dimensions.x}, ${dimensions.y}, ${dimensions.z}}, 
        Variable="${variable.toLowerCase()}",
        LowerDomainBound={${lowerDomainBounds.r}, ${lowerDomainBounds.theta}, ${lowerDomainBounds.phi}}, 
        UpperDomainBound={${upperDomainBounds.r}, ${upperDomainBounds.theta}, ${upperDomainBounds.phi}}, 
        FactorRSquared="${rSquared.toString()}"
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
    const { width, height } = this.props;
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds, volumeProgress } = this.state.data;

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
          <CenteredLabel>{getDirectoryLeaf(this.props.filePaths)}</CenteredLabel>
          <OptionSelect label='Dimensions'
                        options={dimensions} 
                        onChange={(target) => this.onChangeOptionSelect(target, KEY_DIMENSIONS)}/>
          <Variables variable={variable}
                      onChange={this.changeVariable} />
          <OptionSelect label='Lower Domain Bounds'
                        options={lowerDomainBounds} 
                        onChange={(target) => this.onChangeOptionSelect(target, KEY_LOWER_DOMAIN_BOUNDS)}/>
          <OptionSelect label='Upper Domain Bounds'
                        options={upperDomainBounds} 
                        onChange={(target) => this.onChangeOptionSelect(target, KEY_UPPER_DOMAIN_BOUNDS)}/>
          <Checkbox label='Factor r^2?'
                    onChange={this.changeRSquared}/>
          <Button onClick={() => this.upload()}> Convert </Button>
          {this.state.uploadButtonIsClicked && (
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