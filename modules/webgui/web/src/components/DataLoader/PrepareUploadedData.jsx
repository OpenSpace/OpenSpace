import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types'; 

import DataManager from '../../api/DataManager';
import { UploadDataItemScript, ValuePlaceholder } from '../../api/keys';
import { removeLineBreakCharacters } from './utils/helpers';

import styles from './PrepareUploadedData';
import Window from '../common/Window/Window';
import provideWindowWidth from './HOC/provideWindowSize';
import NumericInput from '../common/Input/NumericInput/NumericInput';
import Input from '../common/Input/Input/Input';
import Row from '../common/Row/Row';
import Label from '../common/Label/Label';
import Select from '../common/Input/Select/Select';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      activated: false,
      dimensions: { x: 100, y: 100, z: 100 },
      variable: 'rho'
    };

    this.options = 'R T P RHO UR UT UP BR BT BP JR JT JP'
      .split(' ').map(v => ({ value: v, label: v }));

    this.changeDimensions = this.changeDimensions.bind(this);
    this.upload = this.upload.bind(this);
    this.changeVariable = this.changeVariable.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { filePaths } = this.props;
    const { dimensions } = this.state;

    if( filePaths !== prevProps.filePaths && filePaths !== undefined ) {
      this.setState({ activated: true });
    }
  }


  // Gets the corresponding key of the last changed value in dimensions.
  // Assigns the changed value to the correct key of dimensions.
  changeDimensions({ currentTarget }) {
    let tempDim = this.state.dimensions;
    let key = currentTarget.attributes.label.nodeValue;
    tempDim[key] = Number(currentTarget.value);
    
    this.setState({ dimensions: tempDim });
  }

  upload() {
    const { dimensions, variable } = this.state;
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
    const { dimensions, variable } = this.state;
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
          <Row>
            <Label size={'medium'}>Dimensions: </Label>
            <Row>
              { Object.keys(dimensions).map((key, index) => (
                  <Input 
                    key={key}
                    label={key}
                    placeholder={key}
                    value={dimensions[key]}
                    onChange={this.changeDimensions}
                  />
              ))}
            </Row>
          </Row>
          <Row>
          <Label size={'medium'}>Variable: </Label>
            <Select 
              label={variable === '' ? "Select Variable" : variable}
              options={this.options}
              onChange={this.changeVariable}
              placeholder={''}
            />
          </Row>

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