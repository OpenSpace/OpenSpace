import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types'; 

import styles from './PrepareUploadedData';
import Window from '../common/Window/Window';
import provideWindowWidth from './HOC/provideWindowSize';
import NumericInput from '../common/Input/NumericInput/NumericInput';
import Input from '../common/Input/Input/Input';
import Row from '../common/Row/Row';

class PrepareUploadedData extends Component {
  constructor(props) {
    super(props);

    this.vector = ['x', 'y', 'z'];

    this.state = {
      activated: false,
    };
  }

  componentDidUpdate(prevProps) {
    const { filePaths } = this.props;
    if( filePaths !== prevProps.filePaths && filePaths !== undefined ) {
      this.setState({ activated: true });
    }
  }

  render() {
    const { width, height } = this.props;
    const size = {
      width: width / 2,
      height: height / 2
    }

    return(
      <div className="page-content-wrapper">
        { this.state.activated && (
          <Window
            type="medium"
            title="Prepare Data"
            size={size}
            position={{ x: 100, y: -100 }}
            closeCallback={() => this.setState({ activated: false })}
          >
          { this.vector.map((index) => (
              <Input 
                key={index}
                label={index}
                placeholder={index}
                value={100}
              />
          ))}
            
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