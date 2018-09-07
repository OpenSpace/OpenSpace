import React from 'react';
import { connect } from 'react-redux';
import DataManager from '../../api/DataManager';
import { setSelectedFilePaths, setVolumesConvertedCount, setVolumesToConvertCount, setMetaData, setReadingNewMetaData } from '../../api/Actions/dataLoaderActions';
import { stringListToArray } from './utils/helpers';

class SubscriptionManager extends React.Component {
    constructor(props) {
        super(props);

        this.handleSelectedFiles = this.handleSelectedFiles.bind(this);
        this.handleMetaData = this.handleMetaData.bind(this);
        this.subscribeToFilepaths = this.subscribeToFilepaths.bind(this);
        this.handleVolumesConvertedCount = this.handleVolumesConvertedCount.bind(this);
        this.handleVolumesToConvertCount = this.handleVolumesToConvertCount.bind(this);
        this.handleReadingNewMetaData = this.handleReadingNewMetaData.bind(this);
    }

    componentDidMount() {
        this.subscribeToFilepaths();
    }

    componendWillUnmount() {
        console.log('unmounting subscription man.')
    }

    subscribeToFilepaths() {
        DataManager.subscribe('Modules.DataLoader.Loader.SelectedFiles', this.handleSelectedFiles);
        DataManager.subscribe('Modules.DataLoader.Loader.ReadingNewMetaData', this.handleReadingNewMetaData);
        DataManager.subscribe('Modules.DataLoader.Loader.VolumeMetaDataJSON', this.handleMetaData);
        DataManager.subscribe('Modules.DataLoader.Loader.CurrentVolumesConvertedCount', this.handleVolumesConvertedCount);
        DataManager.subscribe('Modules.DataLoader.Loader.CurrentVolumesToConvertCount', this.handleVolumesToConvertCount);
    }

    handleSelectedFiles(data) {
        this.props.setSelectedFilePaths(stringListToArray(data.Value));
    }

    handleMetaData(data) {
        this.props.setMetaData(data.Value);
    }

    handleVolumesConvertedCount(data) {
        this.props.setVolumesConvertedCount(Number(data.Value));
    }

    handleVolumesToConvertCount(data) {
        this.props.setVolumesToConvertCount(Number(data.Value));
    }

    handleReadingNewMetaData(data) {
        const isReading = data.Value === 'true'
        this.props.setReadingNewMetaData(isReading);
    }

    render() {
        return null;
    }
}

const mapDispatchToProps = dispatch => ({
    setSelectedFilePaths: (filePaths) => {
        dispatch(setSelectedFilePaths(filePaths))
    },
    setMetaData: (stringifiedJson) => {
        dispatch(setMetaData(stringifiedJson))
    },
    setVolumesConvertedCount: (count) => {
        dispatch(setVolumesConvertedCount(count))
    },
    setVolumesToConvertCount: (count) => {
        dispatch(setVolumesToConvertCount(count))
    },
    setReadingNewMetaData: (readingNewMetaData) => {
        dispatch(setReadingNewMetaData(readingNewMetaData))
    }
});

SubscriptionManager = connect(
    null,
    mapDispatchToProps
)(SubscriptionManager);

export default SubscriptionManager;