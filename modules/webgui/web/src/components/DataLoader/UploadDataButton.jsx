import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import Proptypes from 'prop-types'; 

import DataManager from '../../api/DataManager';
import styles from './UploadDataButton';
import { setFilePaths } from '../../api/Actions/dataLoaderActions';
import Button from '../common/Input/Button/Button';
import Label from '../common/Label/Label';

class UploadDataButton extends Component {
  constructor(props) {
		super(props);
		
		this.handleUploadedFiles = this.handleUploadedFiles.bind(this);
	}

	handleClick() {
    this.subscribeToFilepaths();
    this.triggerFilesToUpload();
  }

  triggerFilesToUpload() {
    DataManager.trigger(`Modules.DataLoader.Loader.UploadDataTrigger`)
  }

	subscribeToFilepaths() {
    DataManager.subscribe('Modules.DataLoader.Loader.SelectedFiles', this.handleUploadedFiles);
	}
	
	handleUploadedFiles(data) {
    this.props.setFilePaths(data.Value);
  }
	
	render() {
    return(
      <div>
        <Button onClick={() => this.handleClick()}>
          Load Data
        </Button>
      </div>
    );
	}
}

const mapStateToProps = state => ({
  filePaths: state.dataLoader.filePaths,
});

const mapDispatchToProps = dispatch => ({
  setFilePaths: (filePaths) => {
    dispatch(setFilePaths(filePaths))
  },
});

UploadDataButton = connect(
  mapStateToProps,
  mapDispatchToProps
)(UploadDataButton);

export default UploadDataButton;