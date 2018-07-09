import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import Proptypes from 'prop-types'; 

import DataManager from '../../api/DataManager';
import styles from './UploadDataButton.scss';
import { setSelectedFilePaths, setVolumesConvertedCount, setVolumesToConvertCount } from '../../api/Actions/dataLoaderActions';
import Button from '../common/Input/Button/Button';
import Label from '../common/Label/Label';
import { stringListToArray } from './utils/helpers';

class UploadDataButton extends Component {
  constructor(props) {
		super(props);
		
		this.handleSelectedFiles = this.handleSelectedFiles.bind(this);
		this.handleVolumesConvertedCount = this.handleVolumesConvertedCount.bind(this);
		this.handleVolumesToConvertCount = this.handleVolumesToConvertCount.bind(this);
	}

	handleClick() {
    this.subscribeToFilepaths();
    this.triggerFilesToUpload();
  }

  triggerFilesToUpload() {
    DataManager.trigger(`Modules.DataLoader.Loader.UploadDataTrigger`)
  }

  // This could be in a more fitting component
	subscribeToFilepaths() {
    DataManager.subscribe('Modules.DataLoader.Loader.SelectedFiles', this.handleSelectedFiles);
    DataManager.subscribe('Modules.DataLoader.Loader.CurrentVolumesConvertedCount', this.handleVolumesConvertedCount);
    DataManager.subscribe('Modules.DataLoader.Loader.CurrentVolumesToConvertCount', this.handleVolumesToConvertCount);
	}
	handleSelectedFiles(data) {
    this.props.setSelectedFilePaths(stringListToArray(data.Value));
  }
	handleVolumesConvertedCount(data) {
    this.props.setVolumesConvertedCount(Number(data.Value));
  }
	handleVolumesToConvertCount(data) {
    this.props.setVolumesToConvertCount(Number(data.Value));
  }
	
	render() {
    return(
      <div>
        <Button onClick={() => this.handleClick()}>
          <Label>Upload Data</Label>
        </Button>
      </div>
    );
	}
}

const mapDispatchToProps = dispatch => ({
  setSelectedFilePaths: (filePaths) => {
    dispatch(setSelectedFilePaths(filePaths))
  },
  setVolumesConvertedCount: (count) => {
    dispatch(setVolumesConvertedCount(count))
  },
  setVolumesToConvertCount: (count) => {
    dispatch(setVolumesToConvertCount(count))
  }
});

UploadDataButton = connect(
  null,
  mapDispatchToProps
)(UploadDataButton);

export default UploadDataButton;