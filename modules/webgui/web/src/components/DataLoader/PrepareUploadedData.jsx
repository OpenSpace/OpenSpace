import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types'; 

import DataManager from '../../api/DataManager';
import { UploadDataItemScript, ValuePlaceholder } from '../../api/keys';
import { removeLineBreakCharacters } from './utils/helpers';

import styles from './PrepareUploadedData';
import Window from '../common/Window/Window';
import Checkbox from '../common/Input/Checkbox/Checkbox';
import provideWindowWidth from './HOC/provideWindowSize';
import OptionSelect from './presentational/OptionSelect';
import Variables from './presentational/Variables';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      activated: false,
      dimensions: { x: 100, y: 100, z: 100 },
      lowerDomainBounds: { r: 1, phi: -90, theta: 0 },
      upperDomainBounds: { r: 15, phi: 90, theta: 360 },
      variable: 'rho',
      rSquared: false,
    };

    this.changeDimensions = this.changeDimensions.bind(this);
    this.changeLowerDomainBounds = this.changeLowerDomainBounds.bind(this);
    this.changeUpperDomainBounds = this.changeUpperDomainBounds.bind(this);
    this.changeVariable = this.changeVariable.bind(this);
    this.changeRSquared = this.changeRSquared.bind(this);
    this.upload = this.upload.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { filePaths } = this.props;

    if( filePaths !== prevProps.filePaths && filePaths !== undefined ) {
      this.setState({ activated: true });
    }
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

    this.setState({ domainBounds: tempBound });
  }
  
  changeUpperDomainBounds({ currentTarget }) {
    let tempBound = this.state.upperDomainBounds;
    let key = currentTarget.attributes.label.nodeValue;
    tempBound[key] = Number(currentTarget.value);

    this.setState({ domainBounds: tempBound });
  }

  changeVariable(event) {
    this.setState({ variable: event.value });
  }

  changeRSquared(checked) {
    this.setState({ rSquared: checked });
  }

  upload() {
    const { dimensions, variable, domainBounds, rSquared } = this.state;
    let data = `\'
      return { 
        Dimensions={${dimensions.x}, ${dimensions.y}, ${dimensions.z}}, 
        Variable="${variable.toLowerCase()}" 
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
    const { dimensions, variable, lowerDomainBounds, upperDomainBounds } = this.state;
    const size = {
      width: width / 2,
      height: height / 2
    }
    return(
      <div className="page-content-wrapper">
        { this.state.activated && (
          <Window
            type="small"
            title="Prepare Data"
            size={size}
            position={{ x: 100, y: -100 }}
            closeCallback={() => this.setState({ activated: false })}
          >
          <OptionSelect 
            label={'Dimensions'}
            options={dimensions} 
            onChange={this.changeDimensions}/>
          <Variables 
            variable={variable}
            onChange={this.changeVariable} />
          <OptionSelect 
            label={'Lower Domain Bounds'}
            options={lowerDomainBounds} 
            onChange={this.changeLowerDomainBounds}/>
          <OptionSelect 
            label={'Upper Domain Bounds'}
            options={upperDomainBounds} 
            onChange={this.changeUpperDomainBounds}/>
          <Checkbox 
            label={'Factor R-Squared?'}
            onChange={this.changeRSquared}/>
          <button onClick={() => this.upload()}/>
          </Window>
        )}
      </div>
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